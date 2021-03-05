{-# LANGUAGE LiberalTypeSynonyms #-}

module Compile where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Common.Context.Traverse as Traverse
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Core.FromFrontend as FF
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt as IR (extForgetT)
import qualified Juvix.Core.IR.TransformExt.OnlyExts as IR (injectT)
import qualified Juvix.Core.IR.TransformExt.OnlyExts
import Juvix.Core.IR.Types.Base
import Juvix.Core.Parameterisation
import qualified Juvix.Core.Pipeline as CorePipeline
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as FE
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline as Pipeline
import qualified Michelson.Untyped as Untyped
import Options
import Types

parse :: FilePath -> IO FE.FinalContext
parse fin = do
  core <- Pipeline.toCore ["stdlib/Prelude.ju", "stdlib/Michelson.ju", "stdlib/MichelsonAlias.ju", fin]
  case core of
    Right ctx -> do
      pure ctx
    Left err -> do
      T.putStrLn (show err)
      exitFailure

typecheck ::
  FilePath -> Backend -> IO (ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR)
typecheck fin Michelson = do
  ctx <- parse fin
  let res = Pipeline.contextToCore ctx Param.michelson
  case res of
    Right (FF.CoreDefs order globals) -> do
      let globalDefs = HM.mapMaybe (\case (CoreDef g) -> pure g; _ -> Nothing) globals
      case HM.elems $ HM.filter (\x -> case x of (IR.GFunction (IR.Function (_ :| ["main"]) _ _ _)) -> True; _ -> False) globalDefs of
        [] -> do
          T.putStrLn "No main function found"
          exitFailure
        func@(IR.GFunction (IR.Function name usage ty (IR.FunClause _ [] term _ _ _ :| []))) : [] -> do
          let newGlobals = HM.map (unsafeEvalGlobal (map convGlobal globalDefs)) globalDefs
              inlinedTerm = IR.inlineAllGlobals (IR.injectT term) (\(IR.Global n) -> HM.lookup n globalDefs)
          (res, _) <- exec (CorePipeline.coreToAnn (IR.extForgetT @(Juvix.Core.IR.TransformExt.OnlyExts.T IR.NoExt) inlinedTerm) (IR.globalToUsage usage) ty) Param.michelson newGlobals
          case res of
            Right r -> do
              pure r
            Left err -> do
              print term
              T.putStrLn (show err)
              exitFailure
        somethingElse -> do
          print somethingElse
          exitFailure
    Left err -> do
      print "failed at ctxToCore"
      print err
      exitFailure
typecheck _ _ = exitFailure

compile :: FilePath -> FilePath -> Backend -> IO ()
compile fin fout backend = do
  term <- typecheck fin backend
  print term
  let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
  case res of
    Right c -> do
      T.writeFile fout (M.untypedContractToSource (fst c))
    Left err -> do
      T.putStrLn (show err)
      exitFailure

--unsafeEvalGlobal :: IR.RawGlobal Param.PrimTy Param.RawPrimVal -> Global' (IR.Value Param.PrimTy Param.PrimVal) IR.NoExt Param.PrimTy (Juvix.Core.Parameterisation.TypedPrim Param.PrimTy Param.RawPrimVal)
unsafeEvalGlobal globals g =
  case g of
    -- TODO
    -- GDatatype (Datatype n a l cons) -> GDatatype (Datatype n a l cons)
    -- GDataCon (DataCon n t) -> GDataCon (DataCon n t)
    GFunction (Function n u t cs) -> GFunction (Function n u (unsafeEval globals $ baseToReturn t) (map funClauseEval cs))
    GAbstract (Abstract n u t) -> GAbstract (Abstract n u (unsafeEval globals $ baseToReturn t))

convGlobal g =
  case g of
    GFunction (Function n u t cs) -> GFunction (Function n u (baseToReturn t) (map funClauseReturn cs))
    GAbstract (Abstract n u t) -> GAbstract (Abstract n u (baseToReturn t))

funClauseReturn (FunClause tel patts term rhs catchall unreachable) = FunClause undefined (map pattEval patts) (baseToReturn term) undefined undefined undefined -- TODO

funClauseEval :: IR.FunClause' IR.NoExt Param.PrimTy Param.RawPrimVal -> IR.FunClause' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
funClauseEval (FunClause tel patts term rhs catchall unreachable) = FunClause undefined (map pattEval patts) (baseToReturn term) undefined undefined undefined --TODO

pattEval :: IR.Pattern Param.PrimTy Param.RawPrimVal -> IR.Pattern Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
pattEval patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map pattEval ps)
    IR.PPair x y -> IR.PPair (pattEval x) (pattEval y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn t)
    -- TODO
    IR.PPrim p -> IR.PPrim (CoreApp.Return (Param.Set :| []) p)

baseToReturn :: Term' IR.NoExt Param.PrimTy Param.RawPrimVal -> Term' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
baseToReturn t =
  case t of
    IR.Star u -> IR.Star u
    IR.PrimTy p -> IR.PrimTy p
    IR.Prim p -> IR.Prim (CoreApp.Return (Param.Set :| []) p)
    IR.Pi u x y -> IR.Pi u (baseToReturn x) (baseToReturn y)
    IR.Lam t -> IR.Lam (baseToReturn t)
    IR.Sig u x y -> IR.Sig u (baseToReturn x) (baseToReturn y)
    IR.Pair x y -> IR.Pair (baseToReturn x) (baseToReturn y)
    IR.Let u a b -> IR.Let u (elimToReturn a) (baseToReturn b)
    IR.UnitTy -> IR.UnitTy
    IR.Unit -> IR.Unit
    IR.Elim e -> IR.Elim (elimToReturn e)

elimToReturn :: Elim' IR.NoExt Param.PrimTy Param.RawPrimVal -> Elim' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
elimToReturn e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn e) (baseToReturn t)
    IR.Ann u a b c -> IR.Ann u (baseToReturn a) (baseToReturn b) c

--unsafeEval :: Term' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal) -> Value' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
unsafeEval globals term = (\(Right x) -> x) $ IR.evalTerm (\name -> HM.lookup name globals) term
--  -> (Nothing :: Maybe (Global' (IR.Term Param.PrimTy Param.PrimVal) IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal))))
