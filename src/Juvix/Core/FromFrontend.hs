{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.FromFrontend
  ( module Juvix.Core.FromFrontend,
    module Juvix.Core.FromFrontend.Types,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Generics.SYB as SYB
import qualified Juvix.Core.Common.Context as Ctx
import Juvix.Core.FromFrontend.Types
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.FrontendContextualise as FE
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as FE
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

paramConstant' ::
  P.Parameterisation primTy primVal ->
  FE.Constant ->
  Maybe primVal
paramConstant' p (FE.Number (FE.Double' d)) = P.floatVal p d
paramConstant' p (FE.Number (FE.Integer' i)) = P.intVal p i
paramConstant' p (FE.String (FE.Sho s)) = P.stringVal p s

paramConstant ::
  (HasParam primTy primVal m, HasThrowFF primTy primVal m) =>
  FE.Constant ->
  m primVal
paramConstant k = do
  p <- ask @"param"
  case paramConstant' p k of
    Just x -> pure x
    Nothing -> throwFF $ UnsupportedConstant k

transformTermIR ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m (IR.Term primTy primVal)
transformTermIR q fe = do
  SYB.everywhereM (SYB.mkM transformPatVar) . hrToIR =<< transformTermHR q fe

transformPatVar :: HasPatVars m => IR.Name -> m IR.Name
transformPatVar (IR.Global name) =
  gets @"patVars" $
    maybe (IR.Global name) IR.Pattern
      . HM.lookup name
transformPatVar p = pure p

-- | N.B. doesn't deal with pattern variables since HR doesn't have them.
-- 'transformTermIR' does that.
transformTermHR ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m (HR.Term primTy primVal)
transformTermHR _ (FE.Constant k) = HR.Prim <$> paramConstant k
transformTermHR q (FE.Let l) = transformSimpleLet q l
transformTermHR _ e@(FE.LetType _) = throwFF $ ExprUnimplemented e
transformTermHR _ e@(FE.Match _) = throwFF $ ExprUnimplemented e
transformTermHR q (FE.Name n) = toName <$> lookupSig' (Just q) n
  where
    toName = HR.Elim . HR.Var . maybe n fst
transformTermHR q (FE.Lambda l) = transformSimpleLambda q l
transformTermHR q (FE.Application app) = transformApplication q app
transformTermHR _ (FE.Primitive (FE.Prim p)) = do
  param <- ask @"param"
  maybe (throwFF $ UnknownPrimitive p) pure $
    primTy param p <|> primVal param p
  where
    primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
    primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
transformTermHR _ (FE.List l) =
  throwFF $ ListUnimplemented l
transformTermHR q (FE.Tuple (FE.TupleLit es)) =
  makeTuple <$> traverse (transformTermHR q) es
transformTermHR q (FE.Block (FE.Bloc e)) =
  transformTermHR q e
transformTermHR _ (FE.ExpRecord r) =
  throwFF $ ExpRecordUnimplemented r
transformTermHR q (FE.ArrowE a) = transformArrow q a
transformTermHR q (FE.NamedTypeE (FE.NamedType' _ e)) =
  -- TODO the name will only become relevant (outside of arrows)
  -- when refinements are supported
  transformTermHR q e
transformTermHR _ (FE.RefinedE r) =
  throwFF $ RefinementsUnimplemented r
transformTermHR _ (FE.UniverseName i) =
  -- TODO for universe polymorphism
  throwFF $ UniversesUnimplemented i
transformTermHR q (FE.Parened e) = transformTermHR q e
transformTermHR q (FE.DeclarationE (FE.DeclareExpression _ e)) =
  transformTermHR q e

transformApplication ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Application ->
  m (HR.Term primTy primVal)
transformApplication q (FE.App f xs) =
  getSpecialE q f >>= flip go (toList xs)
  where
    go Nothing xs = do
      f' <- toElim f =<< transformTermHR q f
      HR.Elim . foldl HR.App f' <$> traverse (transformTermHR q) xs
    go (Just s) xs = case s of
      ArrowS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (ArrowS (Just π))) [a, b]
      ArrowS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Pi π x a <$> transformTermHR q b
      PairS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (PairS (Just π))) [a, b]
      PairS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Sig π x a <$> transformTermHR q b
      ColonS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        -- FIXME metavars for usage & universe
        pure $ HR.Elim $ HR.Ann (Usage.SNat 1) a b 0
      TypeS -> do
        ~[i] <- nargs s 1 xs
        HR.Star <$> transformUniverse i
      OmegaS ->
        throwFF UnexpectedOmega
    nargs s n xs
      | length xs == n = pure xs
      | otherwise = throwFF $ WrongNumberBuiltinArgs s n xs

namedArg ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m (NameSymbol.T, HR.Term primTy primVal)
namedArg q e = transformTermHR q e >>= \case
  NamedArgTerm x ty -> pure (NameSymbol.fromSymbol x, ty)
  ty -> pure ("" :| [], ty)

-- TODO implicit arguments???

pattern NamedArgTerm ::
  Symbol -> HR.Term primTy primVal -> HR.Term primTy primVal
pattern NamedArgTerm x ty <-
  HR.Elim (HR.Ann _ (HR.Elim (HR.Var (x :| []))) ty _)

transformSimpleLet ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Let ->
  m (HR.Term primTy primVal)
transformSimpleLet q e@(FE.LetGroup name (clause :| []) body) = do
  let FE.Like args cbody = clause
  args <- traverse isVarArg args
  cbody <- transformTermHR q cbody
  rhs <- toElim (FE.Let e) $ foldr HR.Lam cbody args
  HR.Let Usage.Omega (NameSymbol.fromSymbol name) rhs <$> transformTermHR q body
-- FIXME: usage in frontend let
transformSimpleLet _ e = throwFF $ ExprUnimplemented (FE.Let e)

transformSimpleLambda ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Lambda ->
  m (HR.Term primTy primVal)
transformSimpleLambda q (FE.Lamb pats body) =
  foldr HR.Lam <$> transformTermHR q body <*> traverse isVarPat pats

isVarArg ::
  HasThrowFF primTy primVal m =>
  FE.Arg ->
  m NameSymbol.T
isVarArg (FE.ConcreteA p) = isVarPat p
isVarArg a = throwFF $ ImplicitsUnimplementedA a

isVarPat ::
  HasThrowFF primTy primVal m =>
  FE.MatchLogic ->
  m NameSymbol.T
isVarPat (FE.MatchLogic (FE.MatchName x) Nothing) =
  -- FIXME is a nullary constructor expressed as
  -- @MatchCon n []@ or just as @MatchName n@?
  pure $ NameSymbol.fromSymbol x
isVarPat p = throwFF $ PatternUnimplemented p

transformArrow ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.ArrowExp ->
  m (HR.Term primTy primVal)
transformArrow q f@(FE.Arr' xa π b) =
  case xa of
    FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b
    a -> go π (pure "") a b
  where
    getName (FE.Concrete x) = pure $ NameSymbol.fromSymbol x
    getName (FE.Implicit _) = throwFF $ ImplicitsUnimplemented f
    go π x a b =
      HR.Pi <$> transformUsage q π
        <*> x
        <*> transformTermHR q a
        <*> transformTermHR q b

-- | translate (1,2,3,4) to (1,(2,(3,4)))
makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeTuple [] = HR.Unit
makeTuple [t] = t
makeTuple (t : ts) = HR.Pair t (makeTuple ts)

-- TODO for records:
-- 1) an extended version of core with record literals to begin with
-- 2) translation after typechecking of these to an application of the right
--    constructor to the fields in the originally declared order

toElim ::
  HasThrowFF primTy primVal m =>
  -- | the original expression
  FE.Expression ->
  HR.Term primTy primVal ->
  m (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e -- FIXME add metavar ann

-- TODO put an annotation with metas for the usage/type

isOmega ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m Bool
isOmega q e = (== Just OmegaS) <$> getSpecialE q e

pattern FEIntLit :: Integer -> FE.Expression
pattern FEIntLit i = FE.Constant (FE.Number (FE.Integer' i))

transformUsage ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m Usage.T
transformUsage _ (FEIntLit i) | i >= 0 = pure $ Usage.SNat $ fromInteger i
transformUsage q e = do
  o <- isOmega q e
  if o then pure Usage.Omega else throwFF $ NotAUsage e

transformGUsage ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe FE.Expression ->
  m IR.GlobalUsage
transformGUsage _ Nothing = pure IR.GOmega
transformGUsage _ (Just (FEIntLit 0)) = pure IR.GZero
transformGUsage q (Just e) = do
  o <- isOmega q e
  if o then pure IR.GOmega else throwFF $ NotAGUsage e

transformUniverse ::
  HasThrowFF primTy primVal m =>
  FE.Expression ->
  m IR.Universe
transformUniverse (FEIntLit i) | i >= 0 = pure $ fromIntegral i
transformUniverse e = throwFF $ NotAUniverse e

getSpecial ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (Maybe Special)
getSpecial q x = do
  sig <- lookupSig (Just q) x
  case sig of
    Just (SpecialSig s) -> pure $ Just s
    Just _ -> pure Nothing
    Nothing -> throwFF $ WrongSigType x Nothing

getSpecialE ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m (Maybe Special)
getSpecialE q (FE.Name x) = getSpecial q x
getSpecialE _ _ = pure Nothing

transformSpecialRhs ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Expression ->
  m (Maybe Special)
transformSpecialRhs _ (FE.Primitive (FE.Prim p)) =
  case p of
    "Builtin" :| ["Arrow"] -> pure $ Just $ ArrowS Nothing
    "Builtin" :| ["Pair"] -> pure $ Just $ PairS Nothing
    "Builtin" :| ["Omega"] -> pure $ Just OmegaS
    "Builtin" :| ["Colon"] -> pure $ Just ColonS
    "Builtin" :| ["Type"] -> pure $ Just TypeS
    "Builtin" :| (s : ss) -> throwFF $ UnknownBuiltin $ s :| ss
    _ -> pure Nothing
transformSpecialRhs q (FE.Name x) = getSpecial q x
transformSpecialRhs q (FE.Application (FE.App f (arg :| []))) = do
  head <- getSpecialE q f
  case head of
    Just (ArrowS Nothing) -> Just . ArrowS . Just <$> transformUsage q arg
    Just (PairS Nothing) -> Just . PairS . Just <$> transformUsage q arg
    _ -> pure Nothing
transformSpecialRhs _ _ = pure Nothing

transformSpecial ::
  ( HasThrowFF primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.Final Ctx.Definition ->
  m (Maybe Special)
transformSpecial q def@(Ctx.Def (Ctx.D π ty (FE.Like [] rhs :| []) _)) = do
  rhs <- transformSpecialRhs q rhs
  when (isJust rhs) do
    unless (isNothing π) $ throwFF $ BuiltinWithUsage def
    unless (isNothing ty) $ throwFF $ BuiltinWithTypeSig def
  pure rhs
transformSpecial _ _ = pure Nothing

transformSig ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  m [CoreSigHR primTy primVal]
transformSig x def = trySpecial <||> tryNormal
  where
    q = NameSymbol.mod x
    trySpecial = fmap SpecialSig <$> transformSpecial q def
    tryNormal = transformNormalSig q x def
    x <||> y = x >>= maybe y (pure . pure)

transformNormalSig ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  m [CoreSigHR primTy primVal]
transformNormalSig q x def@(Ctx.Def (Ctx.D π msig _ _)) =
  pure <$> transformValSig q x def π msig
transformNormalSig _ _ (Ctx.Record _) =
  pure [] -- TODO
transformNormalSig q x (Ctx.TypeDeclar typ) =
  transformTypeSig q x typ
transformNormalSig _ _ (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported $ FE.signatureName <$> sig
transformNormalSig q x (Ctx.SumCon (Ctx.Sum {sumTDef})) = do
  let conSig = ConSig {conType = Nothing, conDef = sumTDef}
  let x' = conDefName x
  defSigs <- traverse (transformNormalSig q x' . Ctx.Def) sumTDef
  pure $ conSig : fromMaybe [] defSigs
transformNormalSig _ _ Ctx.CurrentNameSpace =
  pure []
transformNormalSig _ _ (Ctx.Information {}) =
  pure []

conDefName :: NameSymbol.T -> NameSymbol.T
conDefName = NameSymbol.applyBase (<> "$def")

transformTypeSig ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Type ->
  m [CoreSigHR primTy primVal]
transformTypeSig q name dat@(FE.Typ {typeArgs, typeForm}) = do
  (baseTy, hd) <- transformIndices typeForm
  let dataType = foldr makeTPi baseTy typeArgs
  (dataCons, conSigs) <- unzip <$> transformConSigs dat q hd (dataBody typeForm)
  let dataSig = DataSig {dataType, dataCons}
  pure $ dataSig : conSigs
  where
    transformIndices (FE.Arrowed {dataArrow}) = do
      typ <- transformTermHR q dataArrow
      let hd0 = HR.Var name
      let args = HR.Elim . HR.Var . NameSymbol.fromSymbol <$> typeArgs
      pure (typ, Just $ HR.Elim $ foldl HR.App hd0 args)
    transformIndices (FE.NonArrowed {}) =
      pure (HR.Star 0, Nothing) -- TODO metavar for universe

    dataBody (FE.Arrowed {dataAdt'}) = dataAdt'
    dataBody (FE.NonArrowed {dataAdt}) = dataAdt

    makeTPi name res =
      -- TODO metavars for the named args instead of defaulting to types
      -- thin metavars for the named args instead of defaulting to types
      HR.Pi mempty (NameSymbol.fromSymbol name) (HR.Star 0) res

transformConSigs ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  FE.Type -> -- ^ whole type declaration (for error messages)
  NameSymbol.Mod -> -- ^ namespace containing declaration
  Maybe (HR.Term primTy primVal) -> -- ^ datatype head
  FE.Adt -> -- ^ rhs
  m [(NameSymbol.T, CoreSigHR primTy primVal)]
transformConSigs dat pfx hd =
    traverse (transformProduct pfx hd) <=< toProducts
  where
    toProducts (FE.Product (FE.Record r)) = throwFF $ RecordUnimplemented r
    toProducts (FE.Product _) = throwFF $ InvalidDatatype dat
    toProducts (FE.Sum cons) = pure $ map toProduct1 $ toList cons

    toProduct1 (FE.S {sumConstructor, sumValue}) =
      (sumConstructor, fromMaybe (FE.ADTLike []) sumValue)

transformProduct ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe (HR.Term primTy primVal) -> -- ^ datatype head
  (Symbol, FE.Product) ->
  m (NameSymbol.T, CoreSigHR primTy primVal)
transformProduct q hd (x, prod) =
    (NameSymbol.qualify1 q x,) . makeSig <$>
    transformConSig q (NameSymbol.fromSymbol x) hd prod
  where
    makeSig ty = ConSig {conType = Just ty, conDef = Nothing}

transformValSig ::
  ( Data primTy,
    Data primVal,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Maybe Usage.T ->
  Maybe FE.Signature ->
  m (CoreSigHR primTy primVal)
transformValSig q x _ _ (Just (FE.Sig _ π ty cons))
  | null cons = ValSig <$> transformGUsage q π <*> transformTermHR q ty
  | otherwise = throwFF $ ConstraintsUnimplemented x cons
transformValSig _ x def _ _ = throwFF $ SigRequired x def

transformDef ::
  ( Data primTy,
    Data primVal,
    HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  m [CoreDef primTy primVal]
transformDef x def = do
  sig <- lookupSig Nothing x
  case sig of
    Just (SpecialSig s) -> pure [SpecialDef x s]
    _ -> map CoreDef <$> transformNormalDef q x def
      where
        q = NameSymbol.mod x

transformNormalDef ::
  ( Data primTy,
    Data primVal,
    HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  m [IR.RawGlobal primTy primVal]
transformNormalDef q x (Ctx.Def def) = do
  f <- transformFunction q x def
  pure [IR.RawGFunction f]
transformNormalDef _ _ (Ctx.Record _) = pure [] -- TODO
transformNormalDef q x (Ctx.TypeDeclar dec) = transformType q x dec
transformNormalDef _ _ (Ctx.Unknown _) = pure []
transformNormalDef _ _ Ctx.CurrentNameSpace = pure []
transformNormalDef _ _ (Ctx.Information {}) = pure []
transformNormalDef _ _ (Ctx.SumCon {}) = pure []

transformFunction ::
  ( Data primTy,
    Data primVal,
    HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Final' Ctx.Def ->
  m (IR.RawFunction primTy primVal)
transformFunction q x (Ctx.D _ _ def _) = do
  (π, typ) <- getValSig q x
  clauses <- traverse (transformClause q) def
  pure $ IR.RawFunction
    { rawFunName = x,
      rawFunUsage = π,
      rawFunType = hrToIR typ,
      rawFunClauses = clauses
    }

getValSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (IR.GlobalUsage, HR.Term primTy primVal)
getValSig q = getSig' q \case ValSig π ty -> Just (π, ty); _ -> Nothing

getConSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, Maybe (FE.Final' Ctx.Def))
getConSig q = getSig' q \case
  ConSig (Just ty) def -> Just (ty, def); _ -> Nothing

getDataSig ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  m (HR.Term primTy primVal, [NameSymbol.T])
getDataSig q = getSig' q \case DataSig ty cons -> Just (ty, cons); _ -> Nothing

getSig' ::
  ( HasCoreSigs primTy primVal m,
    HasThrowFF primTy primVal m
  ) =>
  NameSymbol.Mod ->
  (CoreSigHR primTy primVal -> Maybe a) ->
  NameSymbol.T ->
  m a
getSig' q f x = do
  msig <- lookupSig (Just q) x
  case msig of
    Just sig | Just ty <- f sig -> pure ty
    _ -> throwFF $ WrongSigType x msig

lookupSig ::
  HasCoreSigs primTy primVal m =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (CoreSig' HR.T primTy primVal))
lookupSig q x = fmap snd <$> lookupSig' q x

lookupSig' ::
  HasCoreSigs primTy primVal m =>
  Maybe NameSymbol.Mod -> -- namespace of current declaration
  NameSymbol.T ->
  m (Maybe (NameSymbol.T, CoreSig' HR.T primTy primVal))
lookupSig' q x' = do
  gets @"coreSigs" \sigs -> do
    let look x = (x,) <$> HM.lookup x sigs
    case q of
      Nothing -> look x
      Just q -> look x <|> look qx
        where
          qx = Ctx.removeTopName $ NameSymbol.qualify q x'
  where
    x = Ctx.removeTopName x'

transformType ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasNextPatVar m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  FE.Type ->
  m [IR.RawGlobal primTy primVal]
transformType q name _ = do
  (ty, conNames) <- getDataSig q name
  let getConSig' x = do (ty, def) <- getConSig q x; pure (x, ty, def)
  conSigs <- traverse getConSig' conNames
  cons <- traverse (uncurry3 $ transformCon q) conSigs
  (args, ℓ) <- splitDataType name ty
  let dat' = IR.RawDatatype {
        rawDataName = name,
        rawDataArgs = args,
        rawDataLevel = ℓ,
        rawDataCons = cons
      }
  pure $ IR.RawGDatatype dat' : fmap IR.RawGDataCon cons

transformCon ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasNextPatVar m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  HR.Term primTy primVal ->
  Maybe (FE.Final' Ctx.Def) ->
  m (IR.RawDataCon primTy primVal)
transformCon q x ty def = do
  def <- traverse (transformFunction q (conDefName x)) def
  pure $ IR.RawDataCon {
    rawConName = x,
    rawConType = hrToIR ty,
    rawConDef = def
  }


splitDataType ::
  HasThrowFF primTy primVal m =>
  NameSymbol.T ->
  HR.Term primTy primVal ->
  m ([IR.RawDataArg primTy primVal], IR.Universe)
splitDataType x ty0 = go ty0
  where
    go (HR.Pi π x s t) = first (arg :) <$> splitDataType x t
      where
        arg =
          IR.RawDataArg
            { rawArgName = x,
              rawArgUsage = π,
              rawArgType = hrToIR s
            }
    go (HR.Star ℓ) = pure ([], ℓ)
    go _ = throwFF $ InvalidDatatypeType x ty0

transformConSig ::
  ( Data primTy,
    Data primVal,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Maybe (HR.Term primTy primVal) ->
  FE.Product ->
  m (HR.Term primTy primVal)
transformConSig _ _ _ (FE.Record r) = throwFF $ RecordUnimplemented r
transformConSig q _ _ (FE.Arrow ty) = transformTermHR q ty
transformConSig _ name Nothing k@(FE.ADTLike {}) =
  throwFF $ InvalidConstructor name k
transformConSig q _ (Just hd) (FE.ADTLike tys) =
    foldrM makeArr hd $ zip names tys
  where
    makeArr (x, arg) res =
      HR.Pi (Usage.SNat 1) x <$> transformTermHR q arg <*> pure res
    names = makeFieldName <$> [0..]
    makeFieldName i = NameSymbol.fromText $ "$field" <> show (i :: Int)

transformClause ::
  ( Data primTy,
    Data primVal,
    HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m
  ) =>
  NameSymbol.Mod ->
  FE.FunctionLike FE.Expression ->
  m (IR.RawFunClause primTy primVal)
transformClause q (FE.Like args body) = do
  put @"patVars" mempty
  put @"nextPatVar" 0
  patts <- traverse transformArg args
  clauseBody <- transformTermIR q body
  pure $ IR.RawFunClause [] patts clauseBody False

transformArg ::
  ( HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m
  ) =>
  FE.Arg ->
  m (IR.Pattern primTy primVal)
transformArg a@(FE.ImplicitA _) = throwFF $ ImplicitsUnimplementedA a
transformArg (FE.ConcreteA pat) = transformPat pat

transformPat ::
  ( HasNextPatVar m,
    HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m
  ) =>
  FE.MatchLogic ->
  m (IR.Pattern primTy primVal)
transformPat (FE.MatchLogic pat _) = case pat of
  -- TODO translate as-patterns into @let@
  FE.MatchCon k pats -> IR.PCon k <$> traverse transformPat pats
  FE.MatchName x -> do
    var <- getNextPatVar
    modify @"patVars" $ HM.insert (NameSymbol.fromSymbol x) var
    pure $ IR.PVar var
  FE.MatchConst p ->
    IR.PPrim <$> paramConstant p
  FE.MatchRecord r ->
    throwFF $ MatchRecordUnimplemented r

getNextPatVar :: HasNextPatVar m => m IR.PatternVar
getNextPatVar = state @"nextPatVar" \v -> (v, succ v)
