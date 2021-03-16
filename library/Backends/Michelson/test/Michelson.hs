module Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpret
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Backends.Michelson.Optimisation
import qualified Juvix.Core.ErasedAnn as J
import Juvix.Library hiding (Type, show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage
import Michelson.Untyped as M hiding (Type)
import qualified Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (show)

--------------------------------------------------------------------------------
-- Test Abstractions
--------------------------------------------------------------------------------

runContract :: RawTerm -> Type -> Either DSL.CompError (Contract' ExpandedOp)
runContract term _ty =
  fst (compileContract term) >>| fst

runExpr :: RawTerm -> Either DSL.CompError EmptyInstr
runExpr term =
  fst (compileExpr term)

runContractWrap :: RawTerm -> Type -> Either DSL.CompError (Contract' ExpandedOp)
runContractWrap term ty =
  runContract (Ann zero newTy (J.LamM [] ["gen%%%"] term)) newTy
  where
    newTy = J.Pi zero (primTy unitPair) ty

interpretExpression :: AnnTerm PrimTy RawPrimVal -> M.Value -> T.TestTree
interpretExpression term equal =
  T.testCase
    (show term <> " :: " <> " should compile to value " <> show equal)
    (Right equal T.@=? (runExpr term >>= Interpret.dummyInterpret))

-- TODO: Switch these tests to use the interpreter (ideally through the parameterisation :) ).
shouldCompile :: RawTerm -> Type -> Text -> T.TestTree
shouldCompile term ty contract =
  T.testCase
    (show term <> " :: " <> show ty <> " should compile to " <> show contract)
    (Right contract T.@=? (untypedContractToSourceLine |<< runContract term ty))

shouldOptimise :: Op -> Op -> T.TestTree
shouldOptimise instr opt =
  T.testCase
    (show instr <> " should optimise to " <> show opt)
    (opt T.@=? optimiseSingle instr)

shouldCompileExpr :: RawTerm -> T.TestTree
shouldCompileExpr term =
  T.testCase
    (show term <> " should compile to an instruction sequence")
    (isRight (fst (compileExpr term)) T.@? "failed to compile")

-- TODO replace this with semantic meaning not exact extraction meaning!
shouldCompileTo :: RawTerm -> [Op] -> T.TestTree
shouldCompileTo term instrs =
  T.testCase
    ("term: " <> show term <> "\n should generate: " <> show instrs <> "\n")
    (DSL.ops (snd (extractTest term)) T.@=? instrs)

--------------------------------------------------------------------------------
-- Test Groups
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "Backend Michelson"
    [ constAppTest,
      pairConstantTest,
      pairNotConstantTest,
      underExactConstTest,
      underExactNonConstTest,
      -- identityFn,
      -- identityApp,
      -- identityApp2,
      --identityExpr,
      optimiseDupDrop,
      optimiseLambdaExec,
      addDoublePairTest,
      constUIntTest,
      overExactConstTest,
      overExactConstTest2,
      overExactNonConstTest,
      overExactNonConstTest2,
      identityTermTest,
      xtwiceTest1,
      xtwiceTest2,
      oddAppTest,
      ifIntTest,
      ifIntConstTest,
      nilTest,
      intListTest
    ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

constAppTest :: T.TestTree
constAppTest = interpretExpression constApp M.ValueUnit

pairNotConstantTest :: T.TestTree
pairNotConstantTest =
  interpretExpression
    pairNotConstant
    (M.ValuePair M.ValueUnit M.ValueUnit)

pairConstantTest :: T.TestTree
pairConstantTest =
  interpretExpression
    pairConstant
    (M.ValuePair M.ValueUnit M.ValueUnit)

optimiseDupDrop :: T.TestTree
optimiseDupDrop =
  shouldOptimise
    (Instructions.dup <> Instructions.drop)
    (M.SeqEx [])

optimiseLambdaExec :: T.TestTree
optimiseLambdaExec =
  shouldOptimise
    (Instructions.lambda Untyped.unit Untyped.unit [] <> Instructions.exec)
    (M.SeqEx [])

identityExpr :: T.TestTree
identityExpr =
  shouldCompileExpr
    identityTerm2

identityExpr2 :: T.TestTree
identityExpr2 =
  shouldCompileExpr
    identityAppExpr

identityExpr3 :: T.TestTree
identityExpr3 =
  shouldCompileExpr
    identityAppExpr2

identityApp2 :: T.TestTree
identityApp2 =
  shouldCompile
    identityAppTerm2
    identityType
    "parameter unit;storage unit;code { { DIG 0;CAR;DIG 0;NIL operation;PAIR } };"

unitTest :: T.TestTree
unitTest =
  shouldCompileExpr unitExpr1

identityFn :: T.TestTree
identityFn =
  shouldCompile
    identityTerm
    identityType
    "parameter unit;storage unit;code { { DIG 0;CAR;NIL operation;PAIR } };"

identityApp :: T.TestTree
identityApp =
  shouldCompile
    identityAppTerm
    identityType
    "parameter unit;storage unit;code { { DIG 0;DIG 0;CAR;NIL operation;PAIR } };"

underExactConstTest :: T.TestTree
underExactConstTest = interpretExpression underExactConst M.ValueUnit

underExactNonConstTest :: T.TestTree
underExactNonConstTest = interpretExpression underExactNonConst M.ValueUnit

addDoublePairTest :: T.TestTree
addDoublePairTest = shouldCompileTo addDoublePairs addDoublePairsAns

constUIntTest :: T.TestTree
constUIntTest = shouldCompileTo constUInt constUIntAns

overExactConstTest :: T.TestTree
overExactConstTest = shouldCompileTo overExactConst overExactConstAns

overExactConstTest2 :: T.TestTree
overExactConstTest2 = interpretExpression overExactConst ValueUnit

overExactNonConstTest2 :: T.TestTree
overExactNonConstTest2 = interpretExpression overExactNonConst ValueUnit

overExactNonConstTest :: T.TestTree
overExactNonConstTest = shouldCompileTo overExactNonConst overExactNonConstAns

identityTermTest :: T.TestTree
identityTermTest = shouldCompileTo identityTerm identityTermAns

xtwiceTest2 :: T.TestTree
xtwiceTest2 =
  interpretExpression xtwice (M.ValueInt 9)

xtwiceTest1 :: T.TestTree
xtwiceTest1 = shouldCompileTo xtwice xtwiceAns

oddAppTest :: T.TestTree
oddAppTest = shouldCompileTo oddApp oddAppAns

ifIntTest :: T.TestTree
ifIntTest = shouldCompileTo ifInt ifIntAns

ifIntConstTest :: T.TestTree
ifIntConstTest = shouldCompileTo ifIntConst ifIntAns

nilTest :: T.TestTree
nilTest = shouldCompileTo nil nilAns

intListTest :: T.TestTree
intListTest = shouldCompileTo intList intListAns

-- dummyTest =
--   runContract identityAppTerm2 identityType
--     >>| Interpret.dummyInterpretContract

--------------------------------------------------------------------------------
-- Terms to test against
--------------------------------------------------------------------------------

-- TODO ∷ promote to a tasty test!
extractTest :: RawTerm -> (Either DSL.CompError M.ExpandedOp, DSL.Env)
extractTest = DSL.execMichelson . runMichelsonExpr

testRun :: (Either DSL.CompError ExpandedOp, DSL.Env)
testRun = extractTest unitExpr1

unitExpr1 :: RawTerm
unitExpr1 =
  Ann
    one
    (primTy Untyped.unit)
    (J.Prim (Constant M.ValueUnit))

symbIdent :: RawTerm
symbIdent =
  Ann one (primTy Untyped.unit) (J.AppM lamxx [unitExpr1])

lamxx :: RawTerm
lamxx =
  Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit)) $
    J.LamM [] ["x"] lookupX

lookupX :: RawTerm
lookupX = Ann one (primTy Untyped.unit) (J.Var "x")

constUInt :: RawTerm
constUInt =
  Ann
    one
    ( J.Pi one (primTy Untyped.unit)
        $ J.Pi
          mempty
          (primTy Untyped.int)
        $ primTy Untyped.unit
    )
    $ J.LamM [] ["x", "y"] lookupX

-- nonConstApp generates:
--   [PrimEx (PUSH @ (Type TInt :) (ValueInt 3))
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (DIG 0)
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

nonConstApp :: RawTerm
nonConstApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      constUInt
      [ push1 M.ValueUnit Untyped.unit,
        push1 (M.ValueInt 3) Untyped.int
      ]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)]
constApp :: RawTerm
constApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM constUInt [unitExpr1, annIntOne 3]

pairGen :: [AnnTerm PrimTy RawPrimVal] -> AnnTerm PrimTy RawPrimVal
pairGen =
  Ann
    one
    (primTy (Untyped.pair Untyped.unit Untyped.unit))
    . J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy
              $ Untyped.pair Untyped.unit Untyped.unit
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )

pairConstant :: RawTerm
pairConstant = pairGen [unitExpr1, unitExpr1]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PAIR : @ % %)]

pairNotConstant :: RawTerm
pairNotConstant = pairGen [unitExpr1, push1 M.ValueUnit Untyped.unit]

-- | 'underExactGen' tests for under application of a multi argument lambda
-- then gives it the exact number of arguments
underExactGen :: RawTerm -> RawTerm
underExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit) $
              primTy Untyped.unit
          )
          $ J.AppM
            ( Ann
                one
                ( J.Pi one (primTy Untyped.unit)
                    $ J.Pi one (primTy Untyped.unit)
                    $ primTy Untyped.unit
                )
                $ J.LamM [] ["x", "y"] lookupX
            )
            [x]
      )
      [x]

-- Generates optimal code!
underExactConst :: RawTerm
underExactConst = underExactGen unitExpr1

-- underExactNonConst generates:

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,PrimEx (DIG 1)]

-- note the dup, this is because in the stack, we pushed it as omega
-- if we did better constant propagation this would be free
underExactNonConst :: RawTerm
underExactNonConst = underExactGen (push1 M.ValueUnit Untyped.unit)

-- | 'overExactGen' tests for overapplication of a multi argument lambda
-- then feeds the rest of the arguments into the inner lambda perfectly
overExactGen :: RawTerm -> RawTerm
overExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy Untyped.unit
          )
          $ J.LamM [] ["y", "z"]
          $ Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit))
          $ J.LamM [] ["x"] lookupX
      )
      [x, x, x]

overExactConst :: RawTerm
overExactConst = overExactGen unitExpr1

overExactNonConst :: RawTerm
overExactNonConst = overExactGen (push1 M.ValueUnit Untyped.unit)

-- IdentityTerm generates

identityTerm :: RawTerm
identityTerm =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          ( J.Pi
              one
              (primTy opl)
              $ J.Pi one (primTy Untyped.unit)
              $ primTy
              $ Untyped.pair opl Untyped.unit
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
        Ann
          one
          (J.PrimTy (PrimTy (M.Type M.TUnit "")))
          $ J.AppM
            ( Ann
                one
                (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.car
            )
            [Ann one (primTy unitPair) (J.Var "x")]
      ]

intPair :: Integer -> Integer -> RawTerm
intPair x y =
  Ann one t $
    J.AppM
      ( Ann one t
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [push1Int x, push1Int y]
  where
    t =
      J.Pi one (primTy int)
        $ J.Pi one (primTy int)
        $ primTy pairInt

-- intPairs1 generates:
-- [PrimEx (PUSH @ (Type TInt :) (ValueInt 3))
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 4))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 5))
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 6))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PAIR : @ % %)]

intPairs1 :: RawTerm
intPairs1 =
  Ann (SNat 2) (primTy (Untyped.pair pairInt pairInt)) $
    J.AppM
      ( Ann one t
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      [intPair 6 5, intPair 4 3]
  where
    t =
      J.Pi one (primTy pairInt)
        $ J.Pi one (primTy pairInt)
        $ primTy
        $ Untyped.pair pairInt pairInt

nil :: RawTerm
nil =
  Nil
    |> J.Prim
    |> Ann one (J.PrimTy (Application List (PrimTy int :| [])))

intList :: RawTerm
intList =
  Ann one (J.PrimTy (Application List (PrimTy int :| []))) $
    J.AppM
      ( Ann one t
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.cons
      )
      [push1Int 3, nil]
  where
    t =
      J.Pi one (primTy int)
        $ J.Pi one (J.PrimTy (Application List (PrimTy int :| [])))
        $ J.PrimTy (Application List (PrimTy int :| []))

-- addPairs "x"
-- [SeqEx []
-- ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
-- ,PrimEx (CAR @ %)
-- ,PrimEx (DIG 1)
-- ,PrimEx (CDR @ %)
-- ,PrimEx (ADD @)]

addPairs :: NameSymbol.T -> RawTerm
addPairs name =
  Ann one t'
    $ J.LamM [] [name]
    $ Ann one t
    $ J.AppM
      ( Ann one (J.Pi one (primTy int) (J.Pi one (primTy int) t))
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.add
      )
      [car int int xLook, cdr int int xLook]
  where
    t = primTy int
    t' = J.Pi (SNat 2) (primTy pairInt) t
    xLook = Ann one (primTy pairInt) (J.Var name)

addDoublePairs :: RawTerm
addDoublePairs =
  Ann one t $
    J.AppM
      ( Ann one (J.Pi (SNat 2) (primTy (Untyped.pair pairInt pairInt)) t)
          $ J.LamM [] ["y"]
          $ Ann one t
          $ J.AppM
            ( Ann one pairIntType
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.pair
            )
            [ applyPlus (car pairInt pairInt xLook) "a",
              applyPlus (cdr pairInt pairInt xLook) "b"
            ]
      )
      [intPairs1]
  where
    t = primTy pairInt
    pairIntType =
      J.Pi one (primTy int)
        $ J.Pi one (primTy int)
        $ primTy pairInt
    xLook =
      Ann one (primTy (Untyped.pair pairInt pairInt)) (J.Var "y")
    applyPlus term name =
      Ann one (primTy int) (J.AppM (addPairs name) [term])

xtwice :: RawTerm
xtwice =
  Ann one (primTy int) $
    J.AppM
      ( Ann
          one
          ( J.Pi mempty (primTy int)
              $ J.Pi (SNat 2) (primTy int)
              $ J.Pi mempty (primTy int)
              $ primTy int
          )
          $ J.LamM [] ["y", "x", "z"]
          $ Ann one (primTy int)
          $ J.AppM
            ( Ann
                one
                ( J.Pi one (primTy int)
                    $ J.Pi one (primTy int)
                    $ primTy int
                )
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.mul
            )
            [ Ann one (primTy int) (J.Var "x"),
              Ann one (primTy int) (J.Var "x")
            ]
      )
      [push1Int 2, pushInt (SNat 2) 3, push1Int 4]

oddApp :: RawTerm
oddApp =
  Ann one (primTy int) $
    J.AppM
      ( Ann
          one
          ( J.Pi mempty (primTy int)
              $ J.Pi (SNat 2) (primTy int)
              $ J.Pi mempty (primTy int)
              $ primTy int
          )
          $ J.LamM [] ["y", "x", "z"]
          $ Ann one (primTy int)
          $ J.AppM
            ( Ann one (J.Pi one (primTy int) (primTy int))
                $ J.LamM ["x"] ["a"]
                $ Ann one (primTy int)
                $ J.AppM
                  ( Ann
                      one
                      ( J.Pi one (primTy int)
                          $ J.Pi one (primTy int)
                          $ primTy int
                      )
                      $ J.Prim
                      $ Instructions.toNewPrimErr Instructions.mul
                  )
                  [ Ann one (primTy int) (J.Var "x"),
                    Ann one (primTy int) (J.Var "a")
                  ]
            )
            [Ann one (primTy int) (J.Var "x")]
      )
      [push1Int 2, push1Int 3, push1Int 4]

-- this should really be a pair we are sending in, but we can let it compile
-- (wrongly typed of course), by instead sending in a non constant unit
identityCall :: AnnTerm PrimTy RawPrimVal
identityCall =
  Ann one (primTy Untyped.unit) $
    J.AppM identityTerm2 [push1Int 3]

identityTerm2 :: RawTerm
identityTerm2 =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          ( J.Pi
              one
              (primTy unitl)
              (J.Pi one (primTy Untyped.unit) (primTy (Untyped.pair unitl Untyped.unit)))
          )
          $ J.Prim
          $ Instructions.toNewPrimErr Instructions.pair
      )
      -- Force the push to be a non constant. This should do nothing
      -- as it's already forced by the second
      [ push1 M.ValueNil unitl,
        car Untyped.unit Untyped.unit
          $ Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit))
          $ J.Var "x"
      ]

primLam :: NonEmpty M.Type -> Type
primLam (ty :| []) = J.PrimTy (PrimTy ty)
primLam (ty :| (t : ts)) = J.Pi one (J.PrimTy (PrimTy ty)) (primLam (t :| ts))

-- [SeqEx []
--   ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
--   ,PrimEx (PUSH @ (Type (TList (Type TOperation :)) :) ValueNil)
--   ,SeqEx [PrimEx (DIG 1),PrimEx (DUP @),PrimEx (DUG 2)]
--   ,PrimEx (CAR @ %)
--   ,PrimEx (PAIR : @ % %)
--   ,PrimEx (DIPN 1 [PrimEx DROP])
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

identityAppTerm :: RawTerm
identityAppTerm =
  Ann one identityType
    $ J.LamM [] ["y"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann one identityType
          $ J.LamM [] ["x"]
          $ Ann one (primTy (Untyped.pair opl Untyped.unit))
          $ J.AppM
            ( Ann one primPairTy $
                J.Prim (Instructions.toNewPrimErr Instructions.pair)
            )
            [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
              car Untyped.unit Untyped.unit
                $ Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit))
                $ J.Var "x"
            ]
      )
      [Ann one (primTy unitPair) (J.Var "y")]

identityAppExpr :: RawTerm
identityAppExpr =
  Ann one identityType
    $ J.LamM [] ["y"]
    $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
    $ J.AppM
      ( Ann one identityType
          $ J.LamM [] ["x"]
          $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
          $ J.AppM
            ( Ann one primPairTy
                $ J.Prim
                $ Instructions.toNewPrimErr Instructions.pair
            )
            [ Ann one (primTy unitl) (J.Prim (Constant M.ValueNil)),
              Ann
                one
                (primTy Untyped.unit)
                $ J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [ Ann
                      one
                      (primTy unitPair)
                      (J.Var "x")
                  ]
            ]
      )
      [Ann one (primTy unitPair) (J.Var "y")]

identityAppTerm2 :: RawTerm
identityAppTerm2 =
  Ann one identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          (J.Pi one primPairTy (primTy (Untyped.pair opl Untyped.unit)))
          $ J.LamM ["x"] ["f"]
          $ Ann one (primTy (Untyped.pair opl Untyped.unit))
          $ J.AppM
            (Ann one primPairTy (J.Var "f"))
            [ Ann one (primTy opl) (J.Prim (Constant M.ValueNil)),
              Ann one (primTy Untyped.unit) $
                J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [Ann one (primTy unitPair) (J.Var "x")]
            ]
      )
      [Ann one primPairTy (J.Prim (Instructions.toNewPrimErr Instructions.pair))]

-- [SeqEx []
--   ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
--   ,PrimEx (CAR @ %)
--   ,PrimEx (NIL : @ (Type TOperation :))
--   ,PrimEx (DIG 1)
--   ,PrimEx (PAIR : @ % %)
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

identityAppExpr2 :: RawTerm
identityAppExpr2 =
  Ann
    one
    identityType
    $ J.LamM [] ["x"]
    $ Ann one (primTy (Untyped.pair opl Untyped.unit))
    $ J.AppM
      ( Ann
          one
          (J.Pi one primPairTy (primTy (Untyped.pair unitl Untyped.unit)))
          $ J.LamM ["x"] ["f"]
          $ Ann one (primTy (Untyped.pair unitl Untyped.unit))
          $ J.AppM
            (Ann one primPairTy (J.Var "f"))
            [ Ann one (primTy unitl) (J.Prim (Constant M.ValueNil)),
              Ann one (primTy Untyped.unit) $
                J.AppM
                  ( Ann
                      one
                      (J.Pi one (primTy unitPair) (primTy Untyped.unit))
                      (J.Prim (Instructions.toNewPrimErr Instructions.car))
                  )
                  [ Ann
                      one
                      (primTy unitPair)
                      (J.Var "x")
                  ]
            ]
      )
      [Ann one primPairTy (J.Prim (Instructions.toNewPrimErr Instructions.pair))]

ifInt :: RawTerm
ifInt =
  Ann
    one
    (primTy Untyped.int)
    $ J.AppM
      (if' Untyped.int)
      [true', push1Int 3, push1Int 4]

ifIntConst :: RawTerm
ifIntConst =
  Ann
    one
    (primTy Untyped.int)
    $ J.AppM
      (if' Untyped.int)
      [true', annIntOne 3, annIntOne 4]

--------------------------------------------------------------------------------
-- Answers to Tests
--------------------------------------------------------------------------------

ifIntAns :: [Op]
ifIntAns =
  [ PrimEx (PUSH "" (M.Type M.TBool "") ValueTrue),
    PrimEx
      ( M.IF
          [PUSH "" (M.Type TInt "") (ValueInt 3) |> PrimEx]
          [PUSH "" (M.Type TInt "") (ValueInt 4) |> PrimEx]
      )
  ]

constUIntAns :: [Op]
constUIntAns =
  [ PrimEx
      ( LAMBDA
          ""
          ( M.Type
              ( TLambda
                  ( M.Type
                      ( TPair
                          ""
                          ""
                          ""
                          ""
                          (M.Type TUnit "")
                          ( M.Type
                              ( TPair
                                  ""
                                  ""
                                  ""
                                  ""
                                  (M.Type TInt "")
                                  (M.Type (TPair "" "" "" "" (M.Type TUnit "") (M.Type TUnit "")) "")
                              )
                              ""
                          )
                      )
                      ""
                  )
                  (M.Type TInt "")
              )
              ""
          )
          (M.Type TInt "")
          [ SeqEx
              [ PrimEx (DUP ""),
                PrimEx (CAR "" ""),
                PrimEx (DIP [PrimEx (CDR "" "")]),
                PrimEx (DIP [SeqEx []])
              ],
            PrimEx (DIG 0),
            PrimEx (DIPN 1 [PrimEx DROP])
          ]
      )
  ]

nilAns :: [Op]
nilAns = [PrimEx (NIL "" "" int)]

intListAns :: [Op]
intListAns =
  nilAns
    <> [ PrimEx (PUSH "" (M.Type TInt "") (ValueInt 3)),
         PrimEx (CONS "")
       ]

-- [SeqEx [PrimEx (DUP @)
--        ,PrimEx (CAR @ %),PrimEx (DIP [PrimEx (CDR @ %)])
--        ,PrimEx (DIP [SeqEx []])]
--   ,PrimEx (DIG 0)
--   ,PrimEx (DIPN 0 [PrimEx DROP])
--   ,PrimEx (DIPN 0 [PrimEx DROP])]

xtwiceAns :: [Op]
xtwiceAns =
  [ PrimEx (PUSH "" (M.Type TInt "") (ValueInt 4)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 3)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 2)),
    SeqEx
      [ PrimEx (DIG 1),
        PrimEx (DUP ""),
        PrimEx (DUG 2)
      ],
    PrimEx (DIG 2),
    PrimEx (MUL ""),
    PrimEx (DIPN 1 [PrimEx DROP]),
    PrimEx (DIPN 1 [PrimEx DROP])
  ]

oddAppAns :: [Op]
oddAppAns =
  [ PrimEx (PUSH "" (M.Type TInt "") (ValueInt 4)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 3)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 2)),
    SeqEx
      [ PrimEx (DIG 1),
        PrimEx (DUP ""),
        PrimEx (DUG 2)
      ],
    PrimEx (DIG 0),
    PrimEx (DIG 2),
    PrimEx (MUL ""),
    PrimEx (DIPN 1 [PrimEx DROP]),
    PrimEx (DIPN 1 [PrimEx DROP])
  ]

addDoublePairsAns :: [Op]
addDoublePairsAns =
  [ PrimEx (PUSH "" (M.Type TInt "") (ValueInt 3)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 4)),
    PrimEx (PAIR "" "" "" ""),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 5)),
    PrimEx (PUSH "" (M.Type TInt "") (ValueInt 6)),
    PrimEx (PAIR "" "" "" ""),
    PrimEx (PAIR "" "" "" ""), -- stack: [((3,4),(5,6))]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP ""),
        PrimEx (DUG 1) --         stack: [((3,4),(5,6)) : ((3,4),(5,6))]
      ],
    PrimEx (CDR "" ""), --        stack: [(3,4) : ((3,4),(5,6))]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP ""),
        PrimEx (DUG 1) --         stack: [(3,4) : (3,4) : ((3,4),(5,6))]
      ],
    PrimEx (CDR "" ""), --        stack: [4 : 3 : ((3,4),(5,6))]
    PrimEx (DIG 1), --            stack: [(3,4) : 3 : ((3,4),(5,6))]
    PrimEx (CAR "" ""), --        stack: [3 : (3,4) : ((3,4),(5,6))]
    PrimEx (ADD ""), --           stack: [7 : ((3,4),(5,6))]
    PrimEx (DIG 1), --            stack: [((3,4),(5,6)) : 7]
    PrimEx (CAR "" ""), --        stack: [(5,6) : 7]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP ""),
        PrimEx (DUG 1) --         stack: [(5,6) : (5,6) : 7]
      ],
    PrimEx (CDR "" ""), --        stack: [5 : 6 : 7]
    PrimEx (DIG 1), --            stack: [(5,6) : 5 : 7]
    PrimEx (CAR "" ""), --        stack: [5 : (5,6) : 7]
    PrimEx (ADD ""), --           stack: [11 : 7]
    PrimEx (PAIR "" "" "" "") --  stack: [(11,7)]
  ]

overExactConstAns :: [Op]
overExactConstAns = [PrimEx (PUSH "" (M.Type TUnit "") ValueUnit)]

overExactNonConstAns :: [Op]
overExactNonConstAns =
  [ PrimEx (PUSH "" (M.Type TUnit "") ValueUnit),
    PrimEx (PUSH "" (M.Type TUnit "") ValueUnit),
    PrimEx (PUSH "" (M.Type TUnit "") ValueUnit),
    PrimEx (DIPN 0 [PrimEx DROP]),
    PrimEx (DIPN 0 [PrimEx DROP]),
    PrimEx (DIG 0)
  ]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (DIPN 1 [PrimEx DROP])
--   ,PrimEx (DIG 1)]

identityTermAns :: [Op]
identityTermAns =
  [ PrimEx
      ( LAMBDA
          ""
          ( M.Type
              ( TLambda
                  ( M.Type
                      ( TPair
                          ""
                          ""
                          ""
                          ""
                          (M.Type TUnit "")
                          ( M.Type
                              ( TPair
                                  ""
                                  ""
                                  ""
                                  ""
                                  ( M.Type
                                      ( TPair
                                          ""
                                          ""
                                          ""
                                          ""
                                          (M.Type TUnit "")
                                          (M.Type TUnit "")
                                      )
                                      ""
                                  )
                                  (M.Type TUnit "")
                              )
                              ""
                          )
                      )
                      ""
                  )
                  (M.Type (TPair "" "" "" "" (M.Type TUnit "") (M.Type TUnit "")) "")
              )
              ""
          )
          (M.Type (TPair "" "" "" "" (M.Type TUnit "") (M.Type TUnit "")) "")
          [ SeqEx [],
            PrimEx (DIG 0),
            PrimEx (CAR "" ""),
            PrimEx (NIL "" "" (M.Type TOperation "")),
            PrimEx (PAIR "" "" "" "")
          ]
      )
  ]

--------------------------------------------------------------------------------
-- Type Abstractions
--------------------------------------------------------------------------------

fstTy :: Type
fstTy =
  J.Pi one (primTy unitPair) (primTy Untyped.unit)

pairTy :: Type
pairTy = primPairTy

identityType :: Type
identityType =
  J.Pi one (primTy unitPair) (primTy (Untyped.pair opl unit))

unitl :: M.Type
unitl = Untyped.list Untyped.unit

unitPair :: M.Type
unitPair = Untyped.pair unit unit

opl :: M.Type
opl = Untyped.list Untyped.operation

unit :: M.Type
unit = Untyped.unit

primPairTy :: Type
primPairTy =
  J.Pi one (primTy opl)
    $ J.Pi one (primTy Untyped.unit)
    $ primTy
    $ Untyped.pair opl Untyped.unit

int :: M.Type
int = Untyped.int

pairInt :: M.Type
pairInt =
  Untyped.pair int int

car :: M.Type -> M.Type -> RawTerm -> RawTerm
car pairFst pairSnd pair =
  Ann one (primTy pairFst) $
    J.AppM
      ( Ann
          one
          (J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairFst))
          (J.Prim (Instructions.toNewPrimErr Instructions.car))
      )
      [pair]

cdr :: M.Type -> M.Type -> RawTerm -> RawTerm
cdr pairFst pairSnd pair =
  Ann one (primTy pairSnd) $
    J.AppM
      ( Ann
          one
          (J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairFst))
          (J.Prim (Instructions.toNewPrimErr Instructions.cdr))
      )
      [pair]

--------------------------------------------------------------------------------
-- general abstractions
--------------------------------------------------------------------------------

primTy :: M.Type -> J.Type PrimTy
primTy = J.PrimTy . PrimTy

annIntOne :: Integer -> RawTerm
annIntOne i =
  Ann one (primTy Untyped.int) (J.Prim (Constant (M.ValueInt i)))

pushInt :: Usage -> Integer -> AnnTerm PrimTy RawPrimVal
pushInt usage i = pushUsage usage (M.ValueInt i) Untyped.int

push1Int :: Integer -> AnnTerm PrimTy RawPrimVal
push1Int i = push1 (M.ValueInt i) Untyped.int

pushUsage :: Usage -> M.Value' Op -> M.Type -> AnnTerm PrimTy RawPrimVal
pushUsage usage const ty =
  Ann
    usage
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann one (J.Pi usage (primTy ty) (primTy ty))
          $ J.Prim
          $ Instructions.toNewPrimErr
          $ Instructions.push ty (M.ValueNil) -- the undefined here is never used
      )
      [Ann one (primTy ty) (J.Prim (Constant const))]

push1 :: M.Value' Op -> M.Type -> AnnTerm PrimTy RawPrimVal
push1 const ty =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann one (J.Pi one (primTy ty) (primTy ty))
          $ J.Prim
          $ Instructions.toNewPrimErr
          $ Instructions.push ty (M.ValueNil) -- the undefined here is never used
      )
      [Ann one (primTy ty) (J.Prim (Constant const))]

true' :: AnnTerm PrimTy RawPrimVal
true' = Ann one (primTy (M.Type M.TBool "")) (J.Prim (Constant M.ValueTrue))

false' :: AnnTerm PrimTy RawPrimVal
false' = Ann one (primTy (M.Type M.TBool "")) (J.Prim (Constant M.ValueFalse))

if' :: M.Type -> AnnTerm PrimTy RawPrimVal
if' ty =
  M.IF [] []
    |> Inst
    |> J.Prim
    |> Ann one (J.Pi one (primTy (M.Type M.TBool "")) (J.Pi one (primTy ty) (J.Pi one (primTy ty) (primTy ty))))
