{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE ViewPatterns #-}

module Juvix.Backends.Michelson.DSL.Interpret where

import qualified Data.Map as Map
import qualified Data.Vinyl.Core as Vinyl
import qualified Fmt
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.DSL.Contract as Contract
import Juvix.Library
import qualified Michelson.Interpret as Interpret
import qualified Michelson.Text as Text
import qualified Michelson.Typed.Aliases as TAlias
import qualified Michelson.Typed.Convert as Convert
import qualified Michelson.Typed.Entrypoints as Entry
import qualified Michelson.Typed.Instr as Instr
import qualified Michelson.Typed.Value as TValue
import qualified Michelson.Untyped.Aliases as Alias
import qualified Michelson.Untyped.Value as Value
import qualified Tezos.Core as Core
import qualified Tezos.Crypto as Crypto

dummyInterpretContract ::
  Alias.Contract -> Either Interpret.InterpretError Interpret.InterpretResult
dummyInterpretContract contract = _
{-
  Interpret.interpret
    contract
    _
    Value.ValueUnit
    Value.ValueUnit
    Contract.dummyContractEnv
  |> Interpret.handleContractReturn
-}

dummyInterpret ::
  Types.EmptyInstr ->
  Either Types.CompilationError Alias.Value
dummyInterpret (Types.EmptyInstr inst) =
  case Interpret.interpretInstr Contract.dummyContractEnv inst Vinyl.RNil of
    Right ((x :: TAlias.Value t) Vinyl.:& _) ->
      untypeValue x
    Left _ -> Left $ Types.InvalidInputType "interpret returned left"
    Right _ -> Left $ Types.InvalidInputType "interpret returned right but mismatched"

-- copied over from Convert!
untypeValue ::
  TValue.Value' Instr.Instr t -> Either Types.CompilationError Alias.Value
untypeValue val =
  case val of
    TValue.VInt i ->
      Right (Value.ValueInt i)
    TValue.VNat i ->
      Right (Value.ValueInt (toInteger i))
    TValue.VString s ->
      Right (Value.ValueString s)
    TValue.VBytes b ->
      Right (Value.ValueBytes (Value.InternalByteString b))
    TValue.VMutez m ->
      Right (Value.ValueInt (toInteger (Core.unMutez m)))
    TValue.VBool True ->
      Right Value.ValueTrue
    TValue.VBool False ->
      Right Value.ValueFalse
    TValue.VKeyHash h ->
      Right (Value.ValueString (Crypto.mformatKeyHash h))
    TValue.VTimestamp t ->
      Right (Value.ValueString (Text.mkMTextUnsafe (Fmt.pretty t)))
    TValue.VAddress a ->
      Right (Value.ValueString (Entry.mformatEpAddress a))
    TValue.VKey b ->
      Right (Value.ValueString (Crypto.mformatPublicKey b))
    TValue.VUnit ->
      Right (Value.ValueUnit)
    TValue.VSignature b ->
      Right (Value.ValueString (Crypto.mformatSignature b))
    TValue.VChainId b ->
      Right (Value.ValueString (Core.mformatChainId b))
    TValue.VOption (Just s) -> do
      s <- untypeValue s
      Right (Value.ValueSome s)
    TValue.VOption Nothing ->
      Right (Value.ValueNone)
    TValue.VList l -> do
      l <- traverse untypeValue l
      Right (vList Value.ValueSeq l)
    TValue.VSet s -> do
      s <- traverse untypeValue (toList s)
      Right (vList Value.ValueSeq s)
    TValue.VContract addr sepc ->
      Entry.sepcName sepc
        |> Entry.EpAddress addr
        |> Entry.mformatEpAddress
        |> Value.ValueString
        |> Right
    TValue.VPair (l, r) -> do
      l <- untypeValue l
      r <- untypeValue r
      Right (Value.ValuePair l r)
    TValue.VOr (Left x) -> do
      x <- untypeValue x
      pure (Value.ValueLeft x)
    TValue.VOr (Right x) -> do
      x <- untypeValue x
      pure (Value.ValueRight x)
    TValue.VLam (TValue.rfAnyInstr -> ops :: Instr.Instr '[inp] '[out]) ->
      Convert.instrToOps ops
        |> vList Value.ValueLambda
        |> Right
    TValue.VMap m -> do
      x <-
        traverse
          ( \(k, v) ->
              Value.Elt <$> untypeValue k <*> untypeValue v
          )
          (Map.toList m)
      pure (vList Value.ValueMap x)
    TValue.VBigMap m -> do
      x <-
        traverse
          ( \(k, v) ->
              Value.Elt <$> untypeValue k <*> untypeValue v
          )
          (Map.toList m)
      pure (vList Value.ValueMap x)
    TValue.VOp _ ->
      Left Types.OpInMichelsonValue
    TValue.VBls12381Fr _ -> _
    TValue.VBls12381G1 _ -> _
    TValue.VBls12381G2 _ -> _
  where
    vList ctor = maybe Value.ValueNil ctor . nonEmpty
