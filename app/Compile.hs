{-# LANGUAGE LiberalTypeSynonyms #-}

module Compile where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Core.FromFrontend as FF
import qualified Juvix.Core.IR as IR
import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import Juvix.Core.Parameterisation
import qualified Juvix.Core.Pipeline as CorePipeline
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as FE
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline as Pipeline
import Options
import qualified System.IO.Temp as Temp
import Types
import qualified Prelude as P

type Code = Text

type OutputCode = Text

type Message = P.String

type Pipeline = Feedback.FeedbackT [] Message IO

-- | Function that parses code gives as input.
parse :: Code -> Pipeline FE.FinalContext
parse code = do
  core <- liftIO $ toCore_wrap code
  case core of
    Right ctx -> return ctx
    Left err -> Feedback.fail $ show err
  where
    toCore_wrap :: Code -> IO (Either Pipeline.Error FE.FinalContext)
    toCore_wrap code = do
      fp <- Temp.writeSystemTempFile "juvix-toCore.ju" (Text.unpack code)
      Pipeline.toCore
        [ "stdlib/Prelude.ju",
          "stdlib/Michelson.ju",
          "stdlib/MichelsonAlias.ju",
          fp
        ]

-- | Perform a type-checking on the given final context, given a specific
-- backend.
typecheck :: Backend -> FE.FinalContext -> Pipeline (ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR)
typecheck Michelson ctx = do
  let res = Pipeline.contextToCore ctx Param.michelson
  case res of
    Right (FF.CoreDefs _order globals) -> do
      let globalDefs = HM.mapMaybe toCoreDef globals
      case HM.elems $ HM.filter isMain globalDefs of
        [] -> Feedback.fail "No main function found"
        [IR.RawGFunction f]
          | IR.RawFunction _name usage ty (clause :| []) <- f,
            IR.RawFunClause _ [] term _ <- clause -> do
            let convGlobals = map convGlobal globalDefs
                newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
                lookupGlobal = IR.rawLookupFun' globalDefs
                inlinedTerm = IR.inlineAllGlobals term lookupGlobal
            (res, _) <- liftIO $ exec (CorePipeline.coreToAnn inlinedTerm (IR.globalToUsage usage) ty) Param.michelson newGlobals
            case res of
              Right r -> do
                pure r
              Left err -> do
                print term
                Feedback.fail $ show err
        somethingElse -> do
          Feedback.fail $ show somethingElse
    Left err -> do
      Feedback.fail $ "failed at ctxToCore\n" ++ show err
typecheck backend _ =
  Feedback.fail $ "Typecheck not implemented for " ++ show backend ++ " backend."

toCoreDef ::
  Alternative f =>
  CoreDef primTy primVal ->
  f (IR.RawGlobal primTy primVal)
toCoreDef (CoreDef g) = pure g
toCoreDef _ = empty

isMain :: RawGlobal' ext primTy primVal -> Bool
isMain (IR.RawGFunction (IR.RawFunction (_ :| ["main"]) _ _ _)) = True
isMain _ = False

-- | Compile the given a backend and annotated terms.
compile :: Backend -> ErasedAnn.AnnTerm Param.PrimTy Param.PrimValHR -> Pipeline OutputCode
compile backend term = do
  print term
  let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
  case res of
    Right c -> do
      return $ M.untypedContractToSource (fst c)
    Left err -> Feedback.fail $ show err

-- | Write the output code to a given file.
writeout :: FilePath -> OutputCode -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code

unsafeEvalGlobal ::
  IR.CanEval IR.NoExt IR.NoExt primTy primVal =>
  IR.RawGlobals primTy primVal ->
  IR.RawGlobal primTy primVal ->
  IR.Global primTy primVal
unsafeEvalGlobal globals g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) -> undefined
    RawGDataCon (RawDataCon n t d) -> undefined
    RawGFunction (RawFunction n u t cs) ->
      GFunction $
        Function n u (unsafeEval globals t) (map (funClauseEval globals) cs)
    RawGAbstract (RawAbstract n u t) ->
      GAbstract $ Abstract n u (unsafeEval globals t)

convGlobal ::
  IR.RawGlobal Param.PrimTy Param.RawPrimVal ->
  IR.RawGlobal Param.PrimTy Param.PrimValIR
convGlobal g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) -> undefined
    RawGDataCon (RawDataCon n t d) -> undefined
    RawGFunction (RawFunction n u t cs) ->
      RawGFunction (RawFunction n u (baseToReturn t) (map funClauseReturn cs))
    RawGAbstract (RawAbstract n u t) ->
      RawGAbstract (RawAbstract n u (baseToReturn t))

funClauseReturn ::
  IR.RawFunClause Param.PrimTy Param.RawPrimVal ->
  IR.RawFunClause Param.PrimTy Param.PrimValIR
funClauseReturn (RawFunClause _tel patts term _catchall) =
  RawFunClause undefined (map pattEval patts) (baseToReturn term) undefined

-- TODO

funClauseEval ::
  IR.RawGlobals primTy primVal ->
  IR.RawFunClause primTy primVal ->
  IR.FunClause primTy primVal
funClauseEval globals (RawFunClause _tel patts rhs _catchall) =
  FunClause undefined patts rhs undefined undefined undefined --TODO

pattEval ::
  IR.Pattern Param.PrimTy Param.RawPrimVal ->
  IR.Pattern Param.PrimTy Param.PrimValIR
pattEval patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map pattEval ps)
    IR.PPair x y -> IR.PPair (pattEval x) (pattEval y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn t)
    -- TODO
    IR.PPrim p -> IR.PPrim (CoreApp.Return (Param.Set :| []) p)

baseToReturn ::
  Term' IR.NoExt Param.PrimTy Param.RawPrimVal ->
  Term' IR.NoExt Param.PrimTy Param.PrimValIR
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

elimToReturn ::
  Elim' IR.NoExt Param.PrimTy Param.RawPrimVal ->
  Elim' IR.NoExt Param.PrimTy (TypedPrim Param.PrimTy Param.RawPrimVal)
elimToReturn e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn e) (baseToReturn t)
    IR.Ann u a b c -> IR.Ann u (baseToReturn a) (baseToReturn b) c

unsafeEval ::
  IR.CanEval IR.NoExt IR.NoExt primTy primVal =>
  IR.RawGlobals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal
unsafeEval globals = fromRight . IR.evalTerm (IR.rawLookupFun' globals)
  where
    fromRight ~(Right x) = x
