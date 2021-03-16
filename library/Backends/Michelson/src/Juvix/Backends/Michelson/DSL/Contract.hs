{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.Backends.Michelson.DSL.Contract where

import Juvix.Library
import qualified Michelson.Interpret as Interpret
import qualified Michelson.TypeCheck.TypeCheck as Type
import qualified Tezos.Address as Address
import qualified Tezos.Core as Core

dummyStamp :: Core.Timestamp
dummyStamp = Core.timestampFromSeconds 1234

dummyStepsLeft :: Interpret.RemainingSteps
dummyStepsLeft = Interpret.RemainingSteps 1000

dummyMutez :: Core.Mutez
dummyMutez = Core.toMutez 123456

dummyAccounts :: Type.TcOriginatedContracts
dummyAccounts = mempty

dummyAddress :: Address.Address
dummyAddress = Address.detGenKeyAddress "dfasfooobarnhilesas"

dummySend :: Core.Mutez
dummySend = Core.toMutez 10000

dummyChainId :: Core.ChainId
dummyChainId = Core.dummyChainId

dummyContractEnv :: Interpret.ContractEnv
dummyContractEnv =
  Interpret.ContractEnv
    { ceNow = dummyStamp,
      ceMaxSteps = dummyStepsLeft,
      ceBalance = dummyMutez,
      ceContracts = dummyAccounts,
      ceSelf = dummyAddress,
      ceSource = dummyAddress,
      ceSender = dummyAddress,
      ceAmount = dummySend,
      ceChainId = dummyChainId,
      ceVotingPowers = _,
      ceOperationHash = _,
      ceGlobalCounter = _,
      ceLevel = _
    }
