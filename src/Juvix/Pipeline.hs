{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context.Traverse as Context
import qualified Juvix.Core.FromFrontend as FF
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (String)

data Error
  = PipeLine Core.Error
  | ParseErr String
  deriving (Show)

toCore :: [FilePath] -> IO (Either Error Target.FinalContext)
toCore paths = do
  x <- Frontend.ofPath paths
  pure $
    case x of
      Left er -> Left (ParseErr er)
      Right x ->
        case Core.ofFrontend x of
          Left errr -> Left (PipeLine errr)
          Right con -> Right con

contextToCore ::
  (Data primTy, Data primVal) =>
  Target.FinalContext ->
  P.Parameterisation primTy primVal ->
  Either (FF.Error primTy primVal) (FF.CoreDefs primTy primVal)
contextToCore ctx param = do
  FF.execEnv ctx param do
    let ordered = Context.recGroups ctx
    for_ ordered \grp -> do
      traverse_ addSig grp
      traverse_ addDef grp
    defs <- get @"core"
    pure $ FF.CoreDefs {defs, order = fmap Context.name <$> ordered}
  where
    addSig (Context.Entry x feDef) = do
      msig <- FF.transformSig x feDef
      for_ msig $ modify @"coreSigs" . HM.insert x
    addDef (Context.Entry x feDef) = do
      defs <- FF.transformDef x feDef
      for_ defs \def ->
        modify @"core" $ HM.insert (defName def) def

defName :: FF.CoreDef primTy primVal -> NameSymbol.T
defName = \case
  FF.CoreDef (IR.GDatatype (IR.Datatype {dataName})) -> dataName
  FF.CoreDef (IR.GDataCon (IR.DataCon {conName})) -> conName
  FF.CoreDef (IR.GFunction (IR.Function {funName})) -> funName
  FF.CoreDef (IR.GAbstract (IR.Abstract {absName})) -> absName
  FF.SpecialDef x _ -> x
