{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Transform where

import Control.Lens (set)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Desugar.Types as Repr
import qualified Juvix.FrontendContextualise.Contextify.Types as Type
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- the name symbols are the modules we are opening
-- TODO ∷ parallelize this
run,
  contextify ::
    Type.Context ->
    (Context.NameSymbol, [Repr.TopLevel]) ->
    IO (Either Context.PathError Type.Pass)
contextify cont (nameSymb, xs) = do
  newNamespace <- Context.switchNameSpace nameSymb cont
  case newNamespace of
    Left errr -> pure (Left errr)
    Right ctx ->
      foldM f Type.P {ctx, opens = [], modsDefined = []} xs
        >>| Right
  where
    f Type.P {ctx, opens, modsDefined} top = do
      Type.P {ctx = ctx', opens = opens', modsDefined = modsDefined'} <-
        updateTopLevel top ctx
      pure
        Type.P
          { ctx = ctx',
            opens = opens <> opens',
            modsDefined = modsDefined <> modsDefined'
          }
run = contextify

-- we can't just have a list, we need to have a map with implicit opens as
-- well...
-- TODO ∷ update this case to register the constructors!
updateTopLevel :: Repr.TopLevel -> Type.Context -> IO Type.Pass
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ dat)) ctx =
  let constructors = collectConstructors dat
      addSum con =
        Context.Sum Nothing name
          |> Context.SumCon
          |> Context.add (NameSpace.Pub con)
      newCtx = foldr addSum ctx constructors
   in pure $
        Type.P
          { ctx = Context.add (NameSpace.Pub name) (Context.TypeDeclar t) newCtx,
            opens = [],
            modsDefined = []
          }
updateTopLevel (Repr.Function (Repr.Func name f sig)) ctx = do
  let precendent =
        case Context.extractValue <$> Context.lookup (pure name) ctx of
          Just (Context.Def Context.D {defPrecedence}) ->
            defPrecedence
          Just (Context.Information info) ->
            case Context.precedenceOf info of
              Just pr -> pr
              Nothing -> Context.default'
          _ -> Context.default'
  (def, modsDefined) <-
    decideRecordOrDef name (Context.currentName ctx) f precendent sig
  pure $
    Type.P
      { ctx = Context.add (NameSpace.Pub name) def ctx,
        opens = [],
        modsDefined
      }
updateTopLevel (Repr.Declaration (Repr.Infixivity dec)) ctx =
  let (name, prec) =
        case dec of
          Repr.AssocL n assoc ->
            (n, Context.Pred Context.Left (fromIntegral assoc))
          Repr.AssocR n assoc ->
            (n, Context.Pred Context.Right (fromIntegral assoc))
          Repr.NonAssoc n assoc ->
            (n, Context.Pred Context.NonAssoc (fromIntegral assoc))
   in pure $ case Context.extractValue <$> Context.lookup (pure name) ctx of
        Just (Context.Def d) ->
          Type.P
            { ctx =
                ctx
                  |> Context.add
                    (NameSpace.Pub name)
                    (Context.Def (d {Context.defPrecedence = prec})),
              opens = [],
              modsDefined = []
            }
        _ ->
          -- TODO ∷
          -- since we aren't doing any reordering, we may come across an
          -- infix declaration first, and thus we should generate an absurd declaration
          -- with reordering or an ordered language this would be obvious
          Type.P
            { ctx =
                Context.add
                  (NameSpace.Pub name)
                  (Context.Information [Context.Prec prec])
                  ctx,
              opens = [],
              modsDefined = []
            }
updateTopLevel (Repr.ModuleOpen (Repr.Open mod)) ctx =
  -- Mod isn't good enough, have to resolve it to the full symbol
  pure $
    Type.P
      { ctx = ctx,
        opens = [mod],
        modsDefined = []
      }
updateTopLevel Repr.TypeClass ctx =
  pure $ Type.P ctx [] []
updateTopLevel Repr.TypeClassInstance ctx =
  pure $ Type.P ctx [] []

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
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  Symbol ->
  NameSymbol.T ->
  NonEmpty (Repr.FunctionLike Repr.Expression) ->
  Context.Precedence ->
  Maybe Repr.Signature ->
  IO (Type.Definition, [NameSymbol.T])
decideRecordOrDef recordName currModName xs pres ty
  | len == 1 && emptyArgs args =
    -- For the two matched cases eventually
    -- turn these into record expressions
    case body of
      Repr.ExpRecord (Repr.ExpressionRecord i) -> do
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at
        -- types
        (nameSpace, innerMods) <- foldM f (NameSpace.empty, []) i
        --
        emptyRecord <- atomically Context.emptyRecord
        --
        let updated = set Context.contents nameSpace . set Context.mTy ty
        --
        pure (Context.Record (updated emptyRecord), newRecordName : innerMods)
        where
          newRecordName = currModName <> pure recordName
          --
          f (nameSpace, prevModNames) (Repr.NonPunned s e) =
            let fieldN = NonEmpty.last s
                like = Repr.Like [] e :| []
             in Nothing
                  |> decideRecordOrDef fieldN newRecordName like Context.default'
                  >>| bimap
                    (\d -> NameSpace.insert (NameSpace.Pub fieldN) d nameSpace)
                    (<> prevModNames)
      Repr.Let _l ->
        def
      _ -> def
  | otherwise = def
  where
    len = length xs
    Repr.Like args body = NonEmpty.head xs
    def = pure (Context.Def (Context.D Nothing ty xs pres), [])

collectConstructors :: Repr.Data -> [Symbol]
collectConstructors dat =
  let adt' =
        case dat of
          Repr.Arrowed _ adt -> adt
          Repr.NonArrowed adt -> adt
      constructors (Repr.Sum sum) =
        NonEmpty.toList (Repr.sumConstructor <$> sum)
      constructors Repr.Product {} = empty
   in constructors adt'

----------------------------------------
-- Helpers
----------------------------------------

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False
