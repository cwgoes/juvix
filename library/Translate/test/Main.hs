module Main where

import qualified Contextualise.Contextify as Contextify
import Contextualise.Infix.ShuntYard (allInfixTests)
import Contextualise.Module.Open (openTests)
import qualified Contextualise.Module.Resolve as Resolve
import qualified Conversion.ML as ML
import Desugar (allDesugar)
import qualified Desugar.Sexp as Sexp
import Golden (contractFiles)
import Juvix.Library (IO)
import qualified Test.Tasty as T

frontEndTests :: T.TestTree
frontEndTests =
  T.testGroup
    "frontend tests"
    [contractFiles]

translationPasses :: T.TestTree
translationPasses =
  T.testGroup
    "translation passes from Frontend to Core"
    [allDesugar, Sexp.top, ML.top]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ frontEndTests,
      allInfixTests,
      openTests,
      Contextify.top,
      Resolve.top,
      translationPasses
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
