module Juvix.Backends.Michelson.DSL.Untyped where

import Juvix.Library ((.), (...), flip)
import qualified Michelson.Untyped.Type as Untyped

type T = Untyped.Type

type UnAnn = Untyped.T

--------------------------------------------------------------------------------
-- Annotated promotion
--------------------------------------------------------------------------------

annotate :: UnAnn -> T
annotate = flip Untyped.Type ""

--------------------------------------------------------------------------------
-- T abstractions
--------------------------------------------------------------------------------

key :: T
key = annotate Untyped.TKey

unit :: T
unit = annotate Untyped.TUnit

signature' :: T
signature' = annotate Untyped.TSignature

chainId :: T
chainId = annotate Untyped.TChainId

option :: T -> T
option = annotate . Untyped.TOption

list :: T -> T
list = annotate . Untyped.TList

set :: T -> T
set = annotate . Untyped.TSet

operation :: T
operation = annotate Untyped.TOperation

contract :: T -> T
contract = annotate . Untyped.TContract

pair :: T -> T -> T
pair = annotate ... Untyped.TPair "" "" "" ""

or :: T -> T -> T
or = annotate ... Untyped.TOr "" ""

lambda :: T -> T -> T
lambda = annotate ... Untyped.TLambda

map :: T -> T -> T
map = annotate ... Untyped.TMap

bigMap :: T -> T -> T
bigMap = annotate ... Untyped.TBigMap

--------------------------------------------------------------------------------
-- Constants May have to promote with either
--------------------------------------------------------------------------------

int, nat, string, bytes, mutez, bool, keyhash, timestamp, address :: T
int = annotate Untyped.TInt
nat = annotate Untyped.TNat
bool = annotate Untyped.TBool
bytes = annotate Untyped.TBytes
mutez = annotate Untyped.TMutez
string = annotate Untyped.TString
keyhash = annotate Untyped.TKeyHash
address = annotate Untyped.TAddress
timestamp = annotate Untyped.TTimestamp
