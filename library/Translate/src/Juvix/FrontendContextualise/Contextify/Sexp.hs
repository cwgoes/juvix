{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Sexp
  ( run,
    contextify,
  )
where

import Control.Lens (set)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendContextualise.Contextify.Types as Type
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

-- the name symbols are the modules we are opening
-- TODO ∷ parallelize this
run,
  contextify ::
    MonadIO m =>
    Type.ContextSexp ->
    (Context.NameSymbol, [Sexp.T]) ->
    m (Either Context.PathError Type.PassSexp)
contextify cont (nameSymb, xs) = do
  newNamespace <- liftIO $ Context.switchNameSpace nameSymb cont
  case newNamespace of
    Left errr -> pure (Left errr)
    Right ctxS ->
      foldM f Type.PS {ctxS, opensS = [], modsDefinedS = []} xs
        |> liftIO
        >>| Right
  where
    f Type.PS {ctxS, opensS, modsDefinedS} top = do
      Type.PS {ctxS = ctx', opensS = opens', modsDefinedS = modsDefined'} <-
        updateTopLevel top ctxS
      pure
        Type.PS
          { ctxS = ctx',
            opensS = opensS <> opens',
            modsDefinedS = modsDefinedS <> modsDefined'
          }
run = contextify

updateTopLevel :: Sexp.T -> Type.ContextSexp -> IO Type.PassSexp
updateTopLevel x ctx
  | Sexp.isAtomNamed x ":type-class" = pure $ Type.PS ctx [] []
  | Sexp.isAtomNamed x ":instance" = pure $ Type.PS ctx [] []
updateTopLevel (name Sexp.:> body) ctx
  | named ":defsig-match" = defun body ctx
  | named "declare" = declare body ctx
  | named "type" = type' body ctx
  | named "open" = open body ctx
  where
    named = Sexp.isAtomNamed name
updateTopLevel _ ctx = pure $ Type.PS ctx [] []

-- | @defun@ takes a defun form and shoves it in a Definition in the
-- top levevl context. Since modules aren't identified yet, we first
-- check to see if they are a module, and if they are a very simple
-- module, then we insert a Record into the Context, and not a definition
defun :: Sexp.T -> Context.T Sexp.T Sexp.T Sexp.T -> IO Type.PassSexp
defun (f Sexp.:> sig Sexp.:> forms) ctx
  | Just name <- eleToSymbol f = do
    let precendent =
          case Context.extractValue <$> Context.lookup (pure name) ctx of
            Just (Context.Def Context.D {defPrecedence}) ->
              defPrecedence
            Just (Context.Information info) ->
              fromMaybe Context.default' (Context.precedenceOf info)
            _ -> Context.default'
        actualSig =
          case sig of
            Sexp.Nil -> Nothing
            ________ -> Just sig
    (def, modsDefinedS) <-
      decideRecordOrDef forms name (Context.currentName ctx) precendent actualSig
    pure $
      Type.PS
        { ctxS = Context.add (NameSpace.Pub name) def ctx,
          opensS = [],
          modsDefinedS
        }
defun _ _ctx = error "malformed defun"

-- | @declare@ takes a declaration and tries to add it to the
-- context. This is a bit tricky, as we could have seen the definition
-- before hand... that part isn't guaranteed, so if we find it then
-- great! Add it to a definition, however if we can't find it, then
-- make a note about the information we have on this symbol, the
-- definition will handle incorporating it
declare :: Sexp.T -> Context.T Sexp.T Sexp.T Sexp.T -> IO Type.PassSexp
declare (Sexp.List [inf, n, i]) ctx
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let prec =
          Context.Pred
            if  | Sexp.isAtomNamed inf "infix" ->
                  Context.NonAssoc
                | Sexp.isAtomNamed inf "infixl" ->
                  Context.Left
                | Sexp.isAtomNamed inf "infixr" ->
                  Context.Right
                | otherwise -> error "malformed declaration"
            (fromIntegral atomNum)
     in pure $ case Context.extractValue <$> Context.lookup (pure atomName) ctx of
          Just (Context.Def d) ->
            Type.PS
              { ctxS =
                  ctx
                    |> Context.add
                      (NameSpace.Pub atomName)
                      (Context.Def (d {Context.defPrecedence = prec})),
                opensS = [],
                modsDefinedS = []
              }
          _ ->
            Type.PS
              { ctxS =
                  Context.add
                    (NameSpace.Pub atomName)
                    (Context.Information [Context.Prec prec])
                    ctx,
                opensS = [],
                modsDefinedS = []
              }
declare _ _ = error "malformed declare"

-- | @type'@ will take its type and add it into the context. Note that
-- since we store information with the type, we will keep the name in
-- the top level form.
type' :: Sexp.T -> Context.T Sexp.T Sexp.T Sexp.T -> IO Type.PassSexp
type' t@(assocName Sexp.:> _ Sexp.:> dat) ctx
  | Just name <- eleToSymbol (Sexp.car assocName) =
    let constructors = collectConstructors dat
        addSum con =
          Context.Sum Nothing name
            |> Context.SumCon
            |> Context.add (NameSpace.Pub con)
        newCtx = foldr addSum ctx constructors
     in pure $
          Type.PS
            { ctxS = Context.add (NameSpace.Pub name) (Context.TypeDeclar t) newCtx,
              opensS = [],
              modsDefinedS = []
            }
type' _ _ = error "malformed type"

-- | @open@ like type will simply take the open and register that the
-- current module is opening it. Since the context does not have such a
-- notion, we have to store this information for the resolve module to
-- properly handle
open :: Sexp.T -> Context.T Sexp.T Sexp.T Sexp.T -> IO Type.PassSexp
open (Sexp.List [mod]) ctx
  | Just Sexp.A {atomName} <- Sexp.atomFromT mod =
    pure $
      Type.PS
        { ctxS = ctx,
          opensS = [atomName],
          modsDefinedS = []
        }
open _ _ = error "malformed open"

-- TODO ∷ why is the context empty?
-- we should somehow note what lists are in scope

-- TODO ∷
-- - once we have type checking, rely on that
-- - for dep types where inference is undecidable, force signature
-- - for functions like (f x) where that evals to a module, where it
--   is dependent but decidable, force signature?

-- The NameSymbol.T return returns back all record names that are found
-- we don't return a map, as we can't do opens in a record

-- | decideRecordOrDef tries to figure out
-- if a given definition is a record or a definition
decideRecordOrDef ::
  Sexp.T ->
  Symbol ->
  NameSymbol.T ->
  Context.Precedence ->
  Maybe Sexp.T ->
  IO (Type.DefinitionSexp, [NameSymbol.T])
decideRecordOrDef xs@(Sexp.List [Sexp.List [Sexp.Nil, body]]) recordName currModName pres ty =
  -- For the two matched cases eventually
  -- turn these into record expressions
  case body of
    name Sexp.:> rest
      | Sexp.isAtomNamed name ":record-no-pun" -> do
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at
        -- types
        (nameSpace, innerMods) <- foldM f (NameSpace.empty, []) grouped
        --
        emptyRecord <- atomically Context.emptyRecord
        --
        let updated = set Context.contents nameSpace . set Context.mTy ty
        --
        pure (Context.Record (updated emptyRecord), newRecordName : innerMods)
      where
        Just grouped = Sexp.toList (Sexp.groupBy2 rest)
        newRecordName = currModName <> pure recordName
        --
        f (nameSpace, prevModNames) (Sexp.List [s, e])
          | Just Sexp.A {atomName} <- Sexp.atomFromT s =
            let fieldN = NonEmpty.last atomName
                like = Sexp.list [Sexp.list [Sexp.Nil, e]]
             in Nothing
                  |> decideRecordOrDef like fieldN newRecordName Context.default'
                  >>| bimap
                    (\d -> NameSpace.insert (NameSpace.Pub fieldN) d nameSpace)
                    (<> prevModNames)
        f x _ = pure x
    _ -> def
  where
    def =
      pure
        ( Context.Def
            (Context.D Nothing ty (Sexp.atom ":lambda-case" Sexp.:> xs) pres),
          []
        )
decideRecordOrDef xs _ _ pres ty =
  pure
    ( Context.Def
        (Context.D Nothing ty (Sexp.atom ":lambda-case" Sexp.:> xs) pres),
      []
    )

collectConstructors :: Sexp.T -> [Symbol]
collectConstructors dat
  | Just s <- Sexp.toList dat =
    let foo = traverse (eleToSymbol . Sexp.car) s
     in case foo of
          Nothing -> []
          Just xs ->
            -- filter out the record constructors, which really aren't constructors
            filter (\x -> x /= ":record-d" && x /= ":") xs
  | otherwise = []

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
