{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

-- |
-- - This module includes a higher level DSL which each instruction
--   has a stack effect
--   + This is similar to the base LLVM bindings we have.
--   + So for example, emitting an =add=, eats two items from the
--     virtual stack, and adds an =Instr.Add= instruction to the
--     sequence of instructions to execute
-- - For constant progoation, have a function say take-2 that looks at
--   the top two items in the stack and then returns back either if
--   they were constants or not and dispatches logic based on that
module Juvix.Backends.Michelson.DSL.InstructionsEff where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Backends.Michelson.DSL.Environment as Env
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import qualified Juvix.Backends.Michelson.DSL.Utils as Utils
import qualified Juvix.Core.ErasedAnn.Types as Ann
import Juvix.Library hiding (abs, and, or, xor)
import qualified Juvix.Library (abs)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as MT
import qualified Michelson.Untyped.Value as V
import Prelude (error)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

instOuter :: Env.Reduction m => Types.RawTerm -> m Instr.ExpandedOp
instOuter a@(Types.Ann _ ty _) = do
  inst <- inst a
  ty <- typeToPrimType ty
  expandedToInst ty inst

expandedToInst :: Env.Reduction m => Untyped.T -> Env.Expanded -> m Instr.ExpandedOp
expandedToInst ty exp =
  case exp of
    Env.Constant c -> do
      let newInstr = VStack.constToInstr ty c
      addInstr newInstr
      pure newInstr
    Env.Expanded op -> pure op
    -- TODO
    Env.MichelsonLam -> error "fails on michLambda"
    Env.Curr c -> mconcat |<< promoteLambda c
    Env.Nop -> pure (Instr.SeqEx [])

inst :: Env.Reduction m => Types.RawTerm -> m Env.Expanded
inst (Types.Ann _usage ty t) =
  case t of
    Ann.Var symbol -> var symbol
    Ann.AppM fun a -> appM fun a
    Ann.UnitM ->
      let unit = Env.Constant V.ValueUnit
       in unit <$ consVal unit ty
    Ann.PairM p1 p2 ->
      appM
        ( Instructions.toNewPrimErr Instructions.pair
            |> Ann.Prim
            |> Ann.Ann
              one
              (Ann.Pi one (Ann.type' p1) (Ann.Pi one (Ann.type' p2) ty))
        )
        [p1, p2]
    Ann.LamM c a b -> do
      v <- lambda c a b ty
      consVal v ty
      pure v
    Ann.Prim prim' ->
      -- Non Instrs will be converted to an Instr via primToFargs
      -- Constants are not functions and thus need to be
      case prim' of
        Types.Constant m -> do
          consVal (Env.Constant m) ty
          pure (Env.Constant m)
        _ ->
          constructPrim prim' ty

applyPrimOnArgs :: Types.RawTerm -> [Types.RawTerm] -> Types.RawTerm
applyPrimOnArgs prim arguments =
  let newTerm = Ann.AppM prim arguments
      retType = Utils.piToReturnType (Ann.type' prim)
   in Ann.Ann one retType newTerm

add,
  mul,
  sub,
  ediv,
  and,
  xor,
  neq,
  eq,
  lt,
  gt,
  le,
  ge,
  neg,
  abs,
  isNat,
  or,
  car,
  cdr,
  cons,
  contract,
  pair ::
    Env.Reduction m => Types.Type -> [Types.RawTerm] -> m Env.Expanded
add = intGen Instructions.add (+)
mul = intGen Instructions.mul (*)
sub = intGen Instructions.sub (-)
and = onBoolGen Instructions.and (&&)
xor = onBoolGen Instructions.xor (/=)
or = onBoolGen Instructions.or (||)
ediv =
  onIntGen
    Instructions.ediv
    ( \x y -> case y of
        0 -> V.ValueNone
        y -> V.ValueSome (V.ValuePair (V.ValueInt (x `div` y)) (V.ValueInt (rem x y)))
    )
eq = onInt1 Instructions.eq (boolToVal . (== 0))
neq = onInt1 Instructions.neq (boolToVal . (/= 0))
le = onInt1 Instructions.le (boolToVal . (<= 0))
lt = onInt1 Instructions.lt (boolToVal . (< 0))
gt = onInt1 Instructions.ge (boolToVal . (>= 0))
ge = onInt1 Instructions.gt (boolToVal . (> 0))
neg = intGen1 Instructions.neg negate
abs = intGen1 Instructions.abs Juvix.Library.abs
car = onPairGen1 Instructions.car fst
cdr = onPairGen1 Instructions.cdr snd
pair = onTwoArgs Instructions.pair (Env.Constant ... V.ValuePair)
contract ty args = do
  primTy <- typeToPrimType ty
  onOneArgsNoConst (Instructions.contract primTy) ty args
cons = onTwoArgsNoConst Instructions.cons
isNat =
  onInt1
    Instructions.isNat
    (\x -> if x >= 0 then V.ValueSome (V.ValueInt x) else V.ValueNone)

lambda ::
  Env.Error m => [NameSymbol.T] -> [NameSymbol.T] -> Types.RawTerm -> Types.Type -> m Env.Expanded
lambda captures arguments body type'
  -- >= as we may return a lambda!
  | length usages >= length arguments =
    pure $
      Env.Curr
        Env.C
          { Env.captures = Set.fromList captures,
            Env.argsLeft = annotatedArgs,
            Env.left = fromIntegral (length arguments),
            Env.ty = type',
            Env.fun =
              Env.Fun (const (inst body <* traverse_ deleteVar annotatedArgs))
          }
  | otherwise =
    throw @"compilationError" $ Types.InvalidInputType "usages/arguments mismatch"
  where
    usages =
      Utils.usageFromType type'
    annotatedArgs =
      zipWith Env.Term arguments usages

var :: (Env.Instruction m, Env.Error m) => NameSymbol.T -> m Env.Expanded
var symb = do
  stack <- get @"stack"
  let pushValueStack value =
        case VStack.lookupType symb stack of
          Just t ->
            modify @"stack"
              ( VStack.cons (VStack.var1E symb (Just value), t)
                  . VStack.predValueUsage symb
              )
          Nothing ->
            throw @"compilationError" (Types.NotInStack symb)
  case VStack.lookupFree symb stack of
    Nothing ->
      throw @"compilationError" (Types.NotInStack symb)
    Just (VStack.Value (VStack.Val' value)) -> do
      pushValueStack (VStack.ConstE value)
      pure (Env.Constant value)
    Just (VStack.Value (VStack.Lam' lamPartial)) -> do
      pushValueStack (VStack.LamPartialE lamPartial)
      pure (Env.Curr lamPartial)
    Just (VStack.Position (VStack.Usage usage _saved) index)
      | usage == one ->
        Env.Nop <$ moveToFront index
      | otherwise ->
        Env.Nop <$ dupToFront index

-- Replaced to always just replace the top element
-- it seems all forms place a lambda at the top!

-- We ignore the usage of the term coming in, however if it's already
-- named, we don't actually change it's usage

-- |
-- Name calls inst, and then determines how best to name the form in the VStack
name :: Env.Reduction m => Env.ErasedTerm -> Types.RawTerm -> m Env.Expanded
name (Env.Term symb usage) f =
  inst f <* modify @"stack" (VStack.nameTop symb usage)

nameSymb :: Env.Reduction m => NameSymbol.T -> Types.RawTerm -> m Env.Expanded
nameSymb symb f@(Types.Ann usage _ _) =
  inst f <* modify @"stack" (VStack.nameTop symb usage)

unboxSingleTypeErr :: Untyped.T -> Untyped.T
unboxSingleTypeErr (MT.Type (MT.TList t) "") = t
unboxSingleTypeErr (MT.Type (MT.TSet t) "") = t
unboxSingleTypeErr (MT.Type (MT.TOption t) "") = t
unboxSingleTypeErr (MT.Type (MT.TContract t) "") = t
unboxSingleTypeErr _ = error "not a type which takes a single type"

unboxDoubleTypeErr :: Untyped.T -> (Untyped.T, Untyped.T)
unboxDoubleTypeErr (MT.Type (MT.TBigMap t1 t2) "") = (t1, t2)
unboxDoubleTypeErr (MT.Type (MT.TLambda t1 t2) "") = (t1, t2)
unboxDoubleTypeErr (MT.Type (MT.TMap t1 t2) "") = (t1, t2)
unboxDoubleTypeErr (MT.Type (MT.TPair _ _ _ _ t1 t2) "") = (t1, t2)
unboxDoubleTypeErr (MT.Type (MT.TOr _ _ t1 t2) "") = (t1, t2)
unboxDoubleTypeErr _ = error "not a type which takes two types"

-- keep this next to primToArgs as they cover the same range!
isNonFunctionPrim :: Types.RawPrimVal -> Bool
isNonFunctionPrim Types.Nil = True
isNonFunctionPrim Types.None = True
isNonFunctionPrim Types.EmptyS = True
isNonFunctionPrim Types.EmptyM = True
isNonFunctionPrim Types.EmptyBM = True
isNonFunctionPrim _ = False

primToArgs :: (Env.Ops m, Env.Error m) => Types.RawPrimVal -> Types.Type -> m ()
primToArgs prim ty =
  let on1 f = f . unboxSingleTypeErr <$> typeToPrimType ty
      on2 f = uncurry f . unboxDoubleTypeErr <$> typeToPrimType ty
   in case prim of
        Types.Nil -> on1 Instructions.nil
        Types.None -> on1 Instructions.none
        Types.EmptyS -> on1 Instructions.emptySet
        Types.EmptyM -> on2 Instructions.emptyMap
        Types.EmptyBM -> on2 Instructions.emptyBigMap
        _ -> error "not a primitive which is not a function"
        >>= addInstr

type RunInstr =
  (forall m. Env.Reduction m => Types.Type -> [Types.RawTerm] -> m Env.Expanded)

primToFargs :: Num b => Types.RawPrimVal -> Types.Type -> Untyped.T -> (Env.Fun, b)
primToFargs (Types.Constant (V.ValueLambda _lam)) _ty _ =
  (undefined, 1)
primToFargs (Types.Inst inst) ty _ =
  (Env.Fun (f (newTy numArgs)), fromIntegral numArgs)
  where
    newTy i = eatType i ty
    numArgs = Instructions.toNumArgs inst
    --
    f :: RunInstr
    f =
      case inst of
        Instr.ADD _ -> add
        Instr.SUB _ -> sub
        Instr.MUL _ -> mul
        Instr.OR {} -> or
        Instr.AND _ -> and
        Instr.XOR _ -> xor
        Instr.EQ {} -> eq
        Instr.NEQ _ -> neq
        Instr.LT {} -> lt
        Instr.LE {} -> le
        Instr.GE {} -> ge
        Instr.GT {} -> gt
        Instr.NEG _ -> neg
        Instr.ABS _ -> abs
        Instr.CAR {} -> car
        Instr.CDR {} -> cdr
        Instr.PAIR {} -> pair
        Instr.EDIV _ -> ediv
        Instr.ISNAT _ -> isNat
        Instr.PUSH {} -> pushConstant
        Instr.IF {} -> evalIf
        Instr.IF_NONE {} -> evalIfNone
        Instr.CONS {} -> cons
        Instr.CONTRACT {} -> contract
-- _ -> error "unspported function in primToFargs"
primToFargs (Types.Constant _) _ _ =
  error "Tried to apply a Michelson Constant"
primToFargs x ty primTy = primToFargs (newPrimToInstrErr x primTy) ty primTy

newPrimToInstrErr :: Types.RawPrimVal -> Untyped.T -> Types.RawPrimVal
newPrimToInstrErr x ty =
  Instructions.toNewPrimErr (instructionOf x ty)

instructionOf :: Types.RawPrimVal -> Untyped.T -> Instr.ExpandedOp
instructionOf x ty =
  case x of
    Types.AddN -> Instructions.add
    Types.AddI -> Instructions.add
    Types.AddTimeStamp -> Instructions.add
    Types.AddMutez -> Instructions.add
    Types.NegN -> Instructions.neg
    Types.NegI -> Instructions.neg
    Types.SubN -> Instructions.sub
    Types.SubI -> Instructions.sub
    Types.SubMutez -> Instructions.sub
    Types.SubTimeStamp -> Instructions.sub
    Types.MulI -> Instructions.mul
    Types.MulN -> Instructions.mul
    Types.MulMutez -> Instructions.mul
    Types.EDivI -> Instructions.ediv
    Types.EDivN -> Instructions.ediv
    Types.EDivMutez -> Instructions.ediv
    Types.OrB -> Instructions.or
    Types.ORI -> Instructions.or
    Types.AndI -> Instructions.and
    Types.AndB -> Instructions.and
    Types.XorI -> Instructions.xor
    Types.XorB -> Instructions.xor
    Types.NotI -> Instructions.not
    Types.NotB -> Instructions.not
    Types.CompareI -> Instructions.compare
    Types.CompareS -> Instructions.compare
    Types.CompareP -> Instructions.compare
    Types.CompareTimeStamp -> Instructions.compare
    Types.CompareMutez -> Instructions.compare
    Types.CompareBytes -> Instructions.compare
    Types.CompareHash -> Instructions.compare
    Types.SizeMap -> Instructions.size
    Types.SizeSet -> Instructions.size
    Types.SizeList -> Instructions.size
    Types.SizeBytes -> Instructions.size
    Types.SizeS -> Instructions.size
    Types.MemSet -> Instructions.mem
    Types.MemMap -> Instructions.mem
    Types.UpdateSet -> Instructions.update
    Types.UpdateMap -> Instructions.update
    Types.UpdateBMap -> Instructions.update
    Types.GetMap -> Instructions.get
    Types.GetBMap -> Instructions.get
    Types.Cons -> Instructions.cons
    Types.Contract -> Instructions.contract ty
    Types.Constant _ -> error "tried to convert a to prim"
    Types.Inst _ -> error "tried to convert an inst to an inst!"

appM :: Env.Reduction m => Types.RawTerm -> [Types.RawTerm] -> m Env.Expanded
appM form@(Types.Ann _u ty t) args =
  let app = inst form >>= flip applyExpanded args
   in case t of
        -- We could remove this special logic, however it would
        -- result in inefficient Michelson!
        Ann.Prim p -> do
          primTy <- typeToPrimType ty
          let (f, lPrim) = primToFargs p ty primTy
           in case length args `compare` lPrim of
                EQ -> Env.unFun f args
                LT -> app
                GT ->
                  throw
                    @"compilationError"
                    (Types.InternalFault "Michelson call with too many args")
        _ -> app

applyExpanded ::
  Env.Reduction m => Env.Expanded -> [Types.RawTerm] -> m Env.Expanded
applyExpanded expanded args =
  case expanded of
    Env.Curr c -> do
      -- we drop the value we've consed onto the top
      modify @"stack" (VStack.drop 1)
      apply c args []
    -- We may get a Michelson lambda if we have one
    -- in storage, make sure to handle this case!
    Env.MichelsonLam -> do
      (elem, ty) <- VStack.car |<< get @"stack"
      --
      let VStack.VarE sym _ (Just VStack.MichelsonLambda) = elem
      --
      applyLambdaFromStorageNArgs (Set.findMin sym) ty args
    Env.Constant _ -> throw @"compilationError" (Types.InternalFault "App on Constant")
    Env.Expanded _ -> throw @"compilationError" (Types.InternalFault "App on Michelson")
    Env.Nop -> throw @"compilationError" (Types.InternalFault "App on NOP")

--------------------------------------------------------------------------------
-- Reduction Helpers for Main functionality
--------------------------------------------------------------------------------

type OnTermGen m f =
  Env.Reduction m =>
  -- function to determine the result
  f ->
  -- Type of the instruction
  Types.Type ->
  -- Arguments
  [Types.RawTerm] ->
  -- Expanded argument
  m Env.Expanded

type OnTerm2Gen m input result =
  OnTermGen m (input -> input -> m result)

type OnTerm1Gen m input result =
  OnTermGen m (input -> m result)

type OnTerm m f =
  Env.Reduction m =>
  -- Instruction to be the operatio
  Instr.ExpandedOp ->
  OnTermGen m f

-- the function on these ones are not on the result
-- but only the constant branch
type OnTerm2 m input result =
  OnTerm m (input -> input -> result)

type OnTerm1 m input result =
  OnTerm m (input -> result)

onBoolGen :: OnTerm2 m Bool Bool
onBoolGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 ->
        let i1 = valToBool instr1
            i2 = valToBool instr2
         in Env.Constant (boolToVal (f i1 i2))
    )

intGen :: OnTerm2 m Integer Integer
intGen op f = onIntGen op (\x y -> V.ValueInt (f x y))

intGen1 :: OnTerm1 m Integer Integer
intGen1 op f = onInt1 op (V.ValueInt . f)

onInt1 :: OnTerm1 m Integer (V.Value' Types.Op)
onInt1 op f =
  onOneArgs
    op
    ( \instr1 ->
        let V.ValueInt i1 = instr1
         in Env.Constant (f i1)
    )

onIntGen :: OnTerm2 m Integer (V.Value' Types.Op)
onIntGen op f =
  onTwoArgs
    op
    ( \instr1 instr2 ->
        let V.ValueInt i1 = instr1
            V.ValueInt i2 = instr2
         in Env.Constant (f i1 i2)
    )

onPairGen1 ::
  OnTerm1 m (V.Value' Types.Op, V.Value' Types.Op) (V.Value' Types.Op)
onPairGen1 op f =
  onOneArgs
    op
    ( \instr1 ->
        let V.ValuePair car cdr = instr1
         in Env.Constant (f (car, cdr))
    )

pushConstant :: Env.Reduction m => Types.Type -> [Types.RawTerm] -> m Env.Expanded
pushConstant =
  onOneArgGen
    ( \instr1 -> do
        addExpanded instr1
        case val instr1 of
          Env.Constant _ -> pure Env.Nop
          _ -> pure (val instr1)
    )

--------------------------------------------------------------------------------
-- On Argument Functions
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Generator functions
------------------------------------------------------------

onTwoArgsGen :: OnTerm2Gen m Protect Env.Expanded
onTwoArgsGen applyOperation typ arguments = do
  -- last argument evaled first
  v <- protectPromotion arguments
  case v of
    instr2 : instr1 : _ -> do
      -- Get the result of the operation on the arguments
      res <- applyOperation instr2 instr1
      -- drop the two arguments on the stack
      modify @"stack" (VStack.drop 2)
      -- add the result to the stack
      consVal res typ
      -- return the operation
      pure res
    _ -> throw @"compilationError" Types.NotEnoughArguments

onOneArgGen :: OnTerm1Gen m Protect Env.Expanded
onOneArgGen applyOperation typ arguments = do
  v <- protectPromotion arguments
  -- see comments onTwoArgsGen
  case v of
    instr1 : _ -> do
      res <- applyOperation instr1
      modify @"stack" (VStack.drop 1)
      consVal res typ
      pure res
    _ -> throw @"compilationError" Types.NotEnoughArguments

-- reverse as we wish to evaluate the last argument first!
protectPromotion :: Env.Reduction m => [Types.RawTerm] -> m [Protect]
protectPromotion =
  traverse (protect . (inst >=> promoteTopStack)) . reverse

------------------------------------------------------------
-- Functions to call from appliers/reducers above
------------------------------------------------------------

onTwoArgsNoConst ::
  Env.Reduction m => Instr.ExpandedOp -> Types.Type -> [Types.RawTerm] -> m Env.Expanded
onTwoArgsNoConst op =
  onTwoArgsGen (\x2 x1 -> noConstantCase op [x2, x1])

onOneArgsNoConst ::
  Env.Reduction m => Instr.ExpandedOp -> Types.Type -> [Types.RawTerm] -> m Env.Expanded
onOneArgsNoConst op =
  onOneArgGen (\x1 -> noConstantCase op [x1])

onTwoArgs :: OnTerm2 m (V.Value' Types.Op) Env.Expanded
onTwoArgs op f =
  onTwoArgsGen
    ( \instr2 instr1 -> do
        -- May be the wrong order?
        let instrs = [instr2, instr1]
        -- check if they are all constants
        -- apply function if so
        if  | allConstants (val <$> instrs) ->
              let Env.Constant i1 = val instr1
                  Env.Constant i2 = val instr2
               in pure (f i1 i2)
            | otherwise -> noConstantCase op [instr2, instr1]
    )

onOneArgs :: OnTerm1 m (V.Value' Types.Op) Env.Expanded
onOneArgs op f =
  onOneArgGen
    ( \instr1 -> do
        -- check if they are all constants
        -- apply function if so
        if  | allConstants [val instr1] ->
              let Env.Constant i1 = val instr1
               in pure (f i1)
            | otherwise -> noConstantCase op [instr1]
    )

------------------------------------------------------------
-- Helper predicate/abstractions for the on functions
------------------------------------------------------------

noConstantCase ::
  Env.Ops m => Instr.ExpandedOp -> [Protect] -> m Env.Expanded
noConstantCase op instrs = do
  -- add the protected instructions
  traverse_ addExpanded instrs
  -- add the op
  addInstr op
  -- return a nop to signal an already added output
  pure Env.Nop

evalIf :: Env.Reduction m => Types.Type -> [Types.RawTerm] -> m Env.Expanded
evalIf typ (bool : thenI : elseI : _) = do
  let eval = inst >=> promoteTopStack
      res = Env.Nop
  _ <- eval bool
  then' <- protect (eval thenI)
  else' <- protect (eval elseI)
  addInstr (Instructions.if' (insts then') (insts else'))
  consVal res typ
  pure res
evalIf _ _ = throw @"compilationError" Types.NotEnoughArguments

evalIfNone :: Env.Reduction m => Types.Type -> [Types.RawTerm] -> m Env.Expanded
evalIfNone typ (bool : thenI : elseI : _) = do
  let eval = inst >=> promoteTopStack
      res = Env.Nop
  _ <- eval bool
  then' <- protect (eval thenI)
  -- This becomes an application
  else' <- protect (constructApplication elseI >>= eval)
  addInstr (Instructions.if' (insts then') (insts else'))
  consVal res typ
  pure res
evalIfNone _ _ = throw @"compilationError" Types.NotEnoughArguments

-- 'constructApplication' takes its first argument and pushes it to the stack
-- used in if_none and if_some, if_cons etc etc etc.
constructApplication ::
  Env.Reduction m =>
  Ann.AnnTerm Types.PrimTy primVal ->
  m (Ann.AnnTerm Types.PrimTy primVal)
constructApplication Ann.Ann {type', term = Ann.LamM {body, arguments}}
  | length (Utils.piToListTy type') == 1 = do
    -- register the value on the stack
    -- ASSUMES unqiue naming
    consVar arg1 Env.Nop usage bound
    pure body
  | otherwise =
    error "unsupported: lambda going through multiple arguments"
  where
    (usage, bound) : _ = Utils.piToList type'
    arg1 : _ = arguments
constructApplication _ = error "unsupported: consturctionApplication on non lambda"

-------------------------------------------------------------------------------
-- Environment Protections, Promotions, and Movements
--------------------------------------------------------------------------------

moveToFront :: (Env.Instruction m, Integral a) => a -> m Instr.ExpandedOp
moveToFront num = do
  let inst = Instructions.dig (fromIntegral num)
  addInstr inst
  modify @"stack" (VStack.dig (fromIntegral num))
  pure inst

-- Now unused
dupToFront :: (Env.Instruction m, Integral a) => a -> m Instr.ExpandedOp
dupToFront num = do
  modify @"stack" (VStack.dupDig (fromIntegral num))
  -- Since the instructions run in reverse if we add it this way
  -- we put dug first
  let instrs =
        Instructions.dig (fromIntegral num)
          <> Instructions.dup
          <> Instructions.dug (succ (fromIntegral num))
  addInstr instrs
  pure instrs

-- Unsafe to implement until we normalize core
copyAndDrop :: Applicative f => p -> f ()
copyAndDrop _i =
  pure ()

data Protect
  = Protect
      { val :: Env.Expanded,
        insts :: [Types.Op]
      }
  deriving (Show)

protect :: Env.Ops m => m Env.Expanded -> m Protect
protect inst = do
  curr <- get @"ops"
  -- Clear the ops, so we only capture the new ops
  put @"ops" []
  v <- inst
  after <- get @"ops"
  put @"ops" curr
  pure Protect {val = v, insts = after}

data ProtectStack
  = ProtectStack
      { prot :: Protect,
        stack :: VStack.T Env.Curried
      }
  deriving (Show)

protectStack :: Env.Instruction m => m Env.Expanded -> m ProtectStack
protectStack inst = do
  curr <- get @"stack"
  prot <- protect inst
  after <- get @"stack"
  put @"stack" curr
  pure ProtectStack {prot = prot, stack = after}

-- Promoting types happen elsewhere
-- so protect just serves to hold the ops effects
addExpanded :: Env.Ops m => Protect -> m ()
addExpanded (Protect _ i) = addInstrs i

promoteTopStack :: Env.Reduction m => Env.Expanded -> m Env.Expanded
promoteTopStack x = do
  stack <- get @"stack"
  (insts, stack') <- VStack.promoteSave 1 stack promoteLambda
  put @"stack" stack'
  addInstrs insts
  pure x

reserveNames :: HasState "count" Word m => Word -> m [NameSymbol.T]
reserveNames i = do
  c <- get @"count"
  put @"count" (i + c)
  pure (NameSymbol.fromString . show <$> [c .. c + i - 1])

-- TODO ∷ drop extra things from the vstack, mainly
-- 1. the function we move to the front
-- 2. the expanded lambda we create
-- Note that it isn't vital, as these aren't stored in the real Michelson stack
-- Other things considered:
-- We don't need to drop the arguments we eval and name, as they should be eaten
-- by the functions they call with the appropriate usages
apply :: Env.Reduction m => Env.Curried -> [Types.RawTerm] -> [NameSymbol.T] -> m Env.Expanded
apply closure args remainingArgs = do
  let totalLength = fromIntegral (length args + length remainingArgs)
  case totalLength `compare` Env.left closure of
    EQ -> do
      -- usage and type doesn't matter here!
      evalArgsAndName
      app
    LT -> do
      evalArgsAndName
      -- make new closure,
      let remaining = Env.left closure - totalLength
          -- remaining ≡ | left |
          -- caputred  ≡ totalLength ≡ | caputreNames | ≡ | tyList |
          (captured, left) = splitAt (fromIntegral totalLength) (Env.argsLeft closure)
          captureNames = fmap Env.name captured
          tyList = take (length captured) (Utils.piToListTy (Env.ty closure))
          con =
            Env.C
              { Env.left = remaining,
                Env.argsLeft = left,
                Env.captures = foldr Set.insert (Env.captures closure) captureNames,
                Env.ty = eatType (fromIntegral totalLength) (Env.ty closure),
                -- we feed in the arguments backwards so we need
                Env.fun = Env.Fun $ \args ->
                  Env.unFun
                    (Env.fun closure)
                    (args <> zipWith makeVar (reverse captured) (reverse tyList))
              }
      consVal (Env.Curr con) (Env.ty con)
      pure (Env.Curr con)
    -- So in this case, we have
    --  | args + remainingArgs | > left/|argsLeft|
    -- We thus need to determine, the args left to eval, do we have enough
    -- names for them?
    -- If so then we can just name them all without reserving any extra names
    -- however if we can't then we need to reserve names first then eval!
    GT ->
      -- figure out which arguments go unnamed, name them
      -- then carry it over to the recursion
      case fromIntegral (length args) `compare` Env.left closure of
        -- This case we have | args | = left\|argsLeft|, so we
        -- can just eval the args and name them, we know, none of the
        -- remaining args get a new name nor are applied, so carry over to
        -- next application
        EQ -> do
          evalArgsAndName
          expanded <- app
          recur expanded remainingArgs
        -- Here we have | args | < left, so we know we have enough names
        -- to name them all, there will be a few names that spill over to
        -- the remaining args, this number is `left - | args |`
        -- Example ∷ argsLeft = [_1, _2]. args = [3], remaining = [_4,_5, ...]
        -- evalArgsAndName ==> [_1 = 3, _2 = _4], but we have [_5, ... remaining
        -- drop (2 - 1) [_4, _5, ...] = [_5 ...]
        LT -> do
          let notRemaining = fromIntegral (Env.left closure) - length args
              newRemaining = drop notRemaining remainingArgs
          evalArgsAndName
          expanded <- app
          recur expanded newRemaining
        -- In this case, we lack enough names for arguments since | args | > left
        -- So we need to see how many arguments are left, this is `| args | - left`
        -- we then need to reserve that many, eval right to left, then append the args
        -- left to the remaining args, since they too need to be applied
        GT -> do
          let unNamed = drop (fromIntegral $ Env.left closure) args
          moreReserved <- reserveNames (fromIntegral (length unNamed))
          traverseNameSymb (zip moreReserved unNamed)
          traverseName (zip (Env.argsLeft closure) args)
          expanded <- app
          recur expanded (moreReserved <> remainingArgs)
  where
    evalArgsAndName = do
      -- with fully saturated args this should take it all
      -- if we are naming an already named var, then the usage does not
      -- changes
      let (toEvalNames, alreadyEvaledNames) = splitAt (length args) (Env.argsLeft closure)
      traverseName (zip toEvalNames args)
      -- thus with fully saturated args this does nothing
      traverse_
        (modify @"stack" . uncurry VStack.addName)
        (zip remainingArgs (Env.name <$> alreadyEvaledNames))
    traverseName = traverse_ (uncurry name) . reverse
    traverseNameSymb = traverse_ (uncurry nameSymb) . reverse
    -- this entire function should be moved to Curried
    -- as this exclusive works on the Curried type
    app =
      Env.ty closure
        |> Utils.piToListTy
        -- we do a reversing ritual as we wish to type the
        -- remaining argument after
        |> reverse
        |> zipWith makeVar (reverse (Env.argsLeft closure))
        -- Undo our last flip, and put the list in the proper place
        |> reverse
        |> Env.unFun (Env.fun closure)
    makeVar (Env.Term name usage) ty = Types.Ann usage ty (Ann.Var name)
    -- TODO ∷ see if we have to drop the top of the stack? It seems to be handled?
    recur (Env.Curr c) xs =
      apply c [] xs
    -- Exec case for Michelson lambdas, e.g. if (storage = f) → g f = f 3!
    recur Env.MichelsonLam _xs =
      applyLambdaFromStorageNArgs undefined undefined args
    recur (Env.Constant _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")
    recur (Env.Expanded _) _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")
    recur Env.Nop _ =
      throw @"compilationError" (Types.InternalFault "apply to non lam")

-- we can only delete things at position greater than 0.
-- this is because if we were to delete 0, then (λx : ω i. x) would error
deleteVar :: Env.Instruction m => Env.ErasedTerm -> m ()
deleteVar (Env.Term name _usage) = do
  stack <- get @"stack"
  let pos = VStack.lookupAllPos name stack
      op i = do
        addInstr (Instructions.dipN (fromIntegral i) [Instructions.drop])
        modify @"stack" (VStack.dropPos i)
      f (VStack.Value _) =
        pure ()
      f (VStack.Position _ 0) = do
        stack <- get @"stack"
        if  | VStack.constantOnTop stack ->
              op 0
            | otherwise ->
              pure ()
      f (VStack.Position _ index) =
        op index
  -- reverse is important here, as we don't decrease the pos of upcoming terms
  -- note that this should only ever hit at most 2 elements, so the
  -- cost of this slightly inefficient generation does not ever matter!
  traverse_ f (reverse pos)

--------------------------------------------------------------------------------
-- Effect Wrangling
--------------------------------------------------------------------------------

addInstrs :: Env.Ops m => [Instr.ExpandedOp] -> m ()
addInstrs x = modify @"ops" (<> x)

addInstr :: Env.Ops m => Instr.ExpandedOp -> m ()
addInstr x = modify @"ops" (<> [x])

--------------------------------------------------------------------------------
-- Boolean Conversions
--------------------------------------------------------------------------------

boolToVal :: Bool -> V.Value' op
boolToVal True = V.ValueTrue
boolToVal False = V.ValueFalse

valToBool :: V.Value' op -> Bool
valToBool V.ValueTrue = True
valToBool V.ValueFalse = False
valToBool _ = error "called valToBool on a non Michelson Bool"

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

constructPrim ::
  Env.Reduction m => Types.RawPrimVal -> Types.Type -> m Env.Expanded
constructPrim prim ty
  -- this one gets Promoted to be on the real stack
  -- TODO ∷ determine if this should be a constant, they should all be in
  -- vstack...
  -- also once we do this, we can remove the StackOps effect of this function!
  | isNonFunctionPrim prim = do
    primToArgs prim ty
    -- cons to show we have it on the stack
    consVal Env.Nop ty
    pure Env.Nop
  -- The final result of this is not promoted
  | otherwise = do
    primTy <- typeToPrimType ty
    let (f, lPrim) = primToFargs prim ty primTy
    names <- reserveNames lPrim
    -- TODO ∷ set the usage of the arguments to 1
    let c =
          Env.Curr $
            Env.C
              { Env.fun = f,
                Env.argsLeft = zipWith Env.Term names (repeat one),
                Env.left = fromIntegral lPrim,
                Env.captures = Set.empty,
                Env.ty = ty
              }
    consVal c ty
    pure c

allConstants :: [Env.Expanded] -> Bool
allConstants = all f
  where
    f (Env.Constant _) = True
    f (Env.Expanded _) = False
    f Env.MichelsonLam = False
    f Env.Curr {} = True
    f Env.Nop = False

expandedToStack :: Env.Expanded -> VStack.Val Env.Curried
expandedToStack (Env.Constant v) = VStack.ConstE v
expandedToStack (Env.Expanded _) = VStack.FuncResultE
expandedToStack (Env.Curr curry) = VStack.LamPartialE curry
expandedToStack Env.MichelsonLam = VStack.MichelsonLambda
expandedToStack Env.Nop = VStack.FuncResultE

consVal :: (Env.Stack m, Env.Error m) => Env.Expanded -> Types.Type -> m ()
consVal result ty = do
  ty <- typeToPrimType ty
  modify @"stack" $
    VStack.cons
      ( VStack.Val
          (expandedToStack result),
        ty
      )

consVarGen ::
  ( HasThrow "compilationError" Env.CompError m,
    HasState "stack" (VStack.T lamType) m
  ) =>
  NameSymbol.T ->
  Maybe (VStack.Val lamType) ->
  Usage.T ->
  Types.Type ->
  m ()
consVarGen symb result usage ty = do
  ty <- typeToPrimType ty
  modify @"stack" $
    VStack.cons
      ( VStack.VarE
          (Set.singleton symb)
          (VStack.Usage usage VStack.notSaved)
          result,
        ty
      )

consVar ::
  (Env.Stack m, Env.Error m) => NameSymbol.T -> Env.Expanded -> Usage.T -> Types.Type -> m ()
consVar symb result = consVarGen symb (Just (expandedToStack result))

consVarNone ::
  (Env.Stack m, Env.Error m) => Env.ErasedTerm -> Types.Type -> m ()
consVarNone (Env.Term symb usage) = consVarGen symb Nothing usage

typeToPrimType :: forall m. Env.Error m => Types.Type -> m Untyped.T
typeToPrimType ty =
  case ty of
    Ann.SymT _ -> throw @"compilationError" $ Types.InvalidInputType "symt cannot be converted to prim type"
    Ann.Star _ -> throw @"compilationError" $ Types.InvalidInputType "star cannot be converted to prim type"
    Ann.PrimTy (Types.PrimTy mTy) -> pure mTy
    Ann.PrimTy (Types.Application arg1 args) -> do
      case arg1 of
        Types.PrimTy {} -> throw @"compilationError" $ Types.InvalidInputType "cannot apply primty"
        Types.Application _ _ -> throw @"compilationError" $ Types.InvalidInputType "cannot apply application"
        _ -> pure ()
      if  | sameLength arg1 args ->
            recurse args
              >>| appPrimTyErr arg1
          | otherwise ->
            throw @"compilationError" $ Types.InvalidInputType "length mismatch"
    -- TODO ∷ Integrate usage information into this
    Ann.Pi _usages argTy retTy -> do
      argTy <- typeToPrimType argTy
      retTy <- typeToPrimType retTy
      pure (Untyped.lambda argTy retTy)
    Ann.PrimTy _ ->
      throw @"compilationError" $ Types.InvalidInputType "cannot convert to primty"
    Ann.Sig _usage fst snd ->
      Untyped.pair <$> typeToPrimType fst <*> typeToPrimType snd
    Ann.UnitTy -> pure Untyped.unit
  where
    recurse = traverse (typeToPrimType . Ann.PrimTy)
    sameLength arg xs =
      lengthType arg == length xs

appPrimTyErr :: Types.PrimTy -> NonEmpty Untyped.T -> Untyped.T
appPrimTyErr Types.Pair (x :| (y : _)) = Untyped.pair x y
appPrimTyErr Types.Lambda (x :| (y : _)) = Untyped.lambda x y
appPrimTyErr Types.Map (x :| (y : _)) = Untyped.map x y
appPrimTyErr Types.BigMap (x :| (y : _)) = Untyped.bigMap x y
appPrimTyErr Types.Option (x :| _) = Untyped.option x
appPrimTyErr Types.Set (x :| _) = Untyped.set x
appPrimTyErr Types.List (x :| _) = Untyped.list x
appPrimTyErr _ _ = error "fail"

lengthType :: Num p => Types.PrimTy -> p
lengthType Types.Pair = 2
lengthType Types.Lambda = 2
lengthType Types.Map = 2
lengthType Types.BigMap = 2
lengthType Types.Option = 1
lengthType Types.List = 1
lengthType Types.Set = 1
lengthType Types.PrimTy {} = 0
lengthType Types.Application {} = 0

eatType :: Natural -> Types.Type -> Types.Type
eatType 0 t = t
eatType x (Ann.Pi _ _ a) = eatType (pred x) a
eatType _ _ = error "Only eat parts of a Pi types, not any other type!"

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

mustLookupType ::
  ( HasState "stack" (VStack.T lamType) m,
    HasThrow "compilationError" Types.CompilationError m
  ) =>
  NameSymbol.T ->
  m Untyped.T
mustLookupType sym = do
  stack <- get @"stack"
  case VStack.lookupType sym stack of
    Just ty -> pure ty
    Nothing ->
      "must be able to find type for symbol: "
        |> (<> show sym)
        |> Types.InternalFault
        |> throw @"compilationError"

-- TODO ∷ figure out why we remove some of the bodies effects
promoteLambda :: Env.Reduction m => Env.Curried -> m [Instr.ExpandedOp]
promoteLambda (Env.C fun argsLeft left captures ty) = do
  -- Step 1: Copy the captures to the top of the stack.
  let capturesList = Set.toList captures
  -- TODO ∷ Figure out how to properly deal with usages here.
  capturesInsts <- flip traverse capturesList $ \c -> do
    ty <- mustLookupType c
    instr <- var c
    pure (ty, instr)
  curr <- get @"stack"
  -- Step 2: Figure out what the stack will be in the body of the function.
  -- Note: these lets are dropping usages the lambda consumes.
  let listOfArgsType = Utils.piToListTy ty
      Just returnType = lastMay listOfArgsType
      termList = reverse $ zip listOfArgsType argsLeft
      stackLeft = VStack.take (length captures) curr
      noVirts = VStack.dropAllVirtual stackLeft
      numberOfExtraArgs = VStack.realItems noVirts
      -- Make sure to run before insts!
      -- Will end up with args ... : captures ... : [] on the stack.
      unpackOps
        | numberOfExtraArgs > 0 =
          Utils.unpackArgsCaptures (fromIntegral left) (fromIntegral numberOfExtraArgs)
        | otherwise =
          Utils.unpackTupleN (fromIntegral (pred left))
  p <- protectStack $ do
    put @"stack" stackLeft
    traverse_ (\(t, term) -> consVarNone term t) termList
    -- Step 3: Compile the body of the lambda.
    insts <-
      -- TODO Clean this up with a helper!
      Env.unFun fun (fmap (\(t, Env.Term sym u) -> Types.Ann u t (Ann.Var sym)) termList)
    returnTypePrim <- typeToPrimType returnType
    _insts <- expandedToInst returnTypePrim insts
    modify @"ops" (unpackOps :)
    pure Env.MichelsonLam
  case p of
    ProtectStack (Protect _val insts) _stack -> do
      -- Step 4: Pack up the captures.
      capturesInsts <- mapM (uncurry expandedToInst) capturesInsts
      -- TODO ∷ Reduce usages of the vstack items, due to eating n from the lambda.
      -- Step 5: find the types of the captures, and generate the type for primArg
      argsWithTypes <- mapM (\(ty, Env.Term sym _u) -> typeToPrimType ty >>| (,) sym) termList
      primReturn <- typeToPrimType returnType
      let capturesTypes =
            (\x -> (x, fromJust (VStack.lookupType x curr)))
              <$> VStack.symbolsInT capturesList curr
          argTy = Utils.lamType capturesTypes argsWithTypes primReturn
          -- Step 6: generate the lambda
          -- TODO ∷ maybe reverse the instrs to lambda that isn't the car cdr dip stuff?
          lambda = Instructions.lambda argTy primReturn insts
      -- Return all operations in order: push the lambda, evaluate captures, pair up captures, APPLY.
      modify @"ops" (lambda :)
      when (numberOfExtraArgs > 0) $
        modify @"ops"
          ( [ Instructions.dip capturesInsts,
              Utils.pairN (numberOfExtraArgs - 1),
              Instructions.apply
            ]
              <>
          )
      get @"ops"

-- Assume lambdas from storage are curried.
applyLambdaFromStorage ::
  Env.Reduction m => NameSymbol.T -> Types.Type -> Types.RawTerm -> m [Instr.ExpandedOp]
applyLambdaFromStorage sym ty arg = do
  ty' <- typeToPrimType ty
  lam <- expandedToInst ty' =<< var sym
  arg <- instOuter arg
  ty <- typeToPrimType (eatType 1 ty)
  let vstackElem = (VStack.varNone "_", ty)
  modify @"stack" (VStack.cons vstackElem . VStack.drop 2)
  pure [lam, arg, Instructions.exec]

applyLambdaFromStorageNArgs ::
  Env.Reduction m => NameSymbol.T -> MT.Type -> [Types.RawTerm] -> m Env.Expanded
applyLambdaFromStorageNArgs _sym _ty _args =
  Env.Expanded . mconcat |<< do
    undefined
