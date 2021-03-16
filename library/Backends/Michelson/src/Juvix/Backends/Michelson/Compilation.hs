-- |
-- - Entrypoints into compilation from core terms to Michelson terms & contracts.
module Juvix.Backends.Michelson.Compilation where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as DSL
import qualified Juvix.Backends.Michelson.Optimisation as Optimisation
import qualified Juvix.Core.ErasedAnn.Types as Ann
import Juvix.Library hiding (Type)
import qualified Michelson.Printer as M
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M

typedContractToSource :: M.SomeContract -> Text
typedContractToSource (M.SomeContract (MT.Contract {cCode = instr})) =
  L.toStrict (M.printTypedContractCode False instr)

untypedContractToSource :: M.Contract' M.ExpandedOp -> Text
untypedContractToSource c = L.toStrict (M.printUntypedContract False c)

untypedContractToSourceLine :: M.Contract' M.ExpandedOp -> Text
untypedContractToSourceLine c = L.toStrict (M.printUntypedContract True c)

compileContract ::
  RawTerm ->
  (Either DSL.CompError (M.Contract' M.ExpandedOp, M.SomeContract), [CompilationLog])
compileContract term =
  let (ret, env) = DSL.execMichelson (compileToMichelsonContract term)
   in (ret, DSL.compilationLog env)

compileExpr :: RawTerm -> (Either DSL.CompError EmptyInstr, [CompilationLog])
compileExpr term =
  let (ret, env) = DSL.execMichelson (compileToMichelsonExpr term)
   in (ret, DSL.compilationLog env)

compileToMichelsonContract ::
  DSL.Reduction m =>
  RawTerm ->
  m (M.Contract' M.ExpandedOp, M.SomeContract)
compileToMichelsonContract term = do
  let Ann.Ann _ ty _ = term
  michelsonTy <- DSL.typeToPrimType ty
  case michelsonTy of
    M.Type (M.TLambda argTy _) _
      | (M.Type (M.TPair _ _ _ _ paramTy storageTy) _) <- argTy -> do
        let Ann.Ann _ (Ann.Pi argUsage _ _) (Ann.LamM _ [name] body) = term
        let paramTy' = M.ParameterType paramTy (M.ann "")
        modify @"stack"
          ( VStack.cons
              ( VStack.VarE
                  (Set.singleton name)
                  (VStack.Usage argUsage VStack.notSaved)
                  Nothing,
                argTy
              )
          )
        --
        _ <- DSL.instOuter body
        -- HAXX
        -- modify @"stack" ((\(x : y : zs) -> x : zs))
        modify @"ops" (\x -> x <> [Instructions.dip [Instructions.drop]])
        -- END HAXX
        michelsonOp' <- mconcat |<< get @"ops"
        let michelsonOp = michelsonOp'
        --
        let contract = M.Contract paramTy' storageTy [michelsonOp] M.PSC
        let tcOpts = M.TypeCheckOptions {tcVerbose = False}
        --
        case M.typeCheckContract contract tcOpts of
          Right _ -> do
            optimised <- Optimisation.optimise michelsonOp
            let optimisedContract =
                  M.Contract paramTy' storageTy [optimised] M.PSC
            case M.typeCheckContract optimisedContract tcOpts of
              Right c ->
                pure (optimisedContract, c)
              Left err ->
                throw @"compilationError"
                  (DidNotTypecheckAfterOptimisation optimised err)
          Left err ->
            throw @"compilationError" (DidNotTypecheck michelsonOp err)
    ty ->
      throw @"compilationError" (InvalidInputType $ show ty)

compileToMichelsonExpr ::
  DSL.Reduction m =>
  RawTerm ->
  m EmptyInstr
compileToMichelsonExpr term = do
  _ <- DSL.instOuter term
  michelsonOp <- mconcat |<< get @"ops"
  case M.runTypeCheckIsolated (M.typeCheckList [michelsonOp] M.SNil) of
    Right (_ M.:/ (s M.::: _)) -> pure (EmptyInstr s)
    -- TODO ∷ Figure out what this case should be
    Right (_ M.:/ (M.AnyOutInstr _)) -> undefined
    Left err -> throw @"compilationError" (DidNotTypecheck michelsonOp err)

runMichelsonExpr :: DSL.Reduction m => RawTerm -> m M.ExpandedOp
runMichelsonExpr = DSL.instOuter
