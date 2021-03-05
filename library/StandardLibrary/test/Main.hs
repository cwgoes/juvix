module Main where

import Juvix.Library
import qualified NameSymb
import qualified Sexp
import qualified Sexp.Parser
import qualified Sexp.SimplifiedPasses
import qualified Pretty
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup "All tests that are checked" [
    NameSymb.top,
    Sexp.top,
    Sexp.Parser.top,
    Sexp.SimplifiedPasses.top,
    Pretty.top
  ]

main :: IO ()
main = T.defaultMain allCheckedTests
