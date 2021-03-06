module Main where

import Common.Context (contextTests)
import Conv (coreConversions)
import Erasure (erasureTests)
import qualified IR.Weak as Weak
import qualified HR.Pretty
import Juvix.Library (IO)
import Parser (coreParser)
import qualified Test.Tasty as T
import Typechecker (coreCheckerEval)

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ coreCheckerEval,
      coreConversions,
      coreParser
    ]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup "All tests that are checked" [
    coreTests,
    erasureTests,
    contextTests,
    Weak.top,
    HR.Pretty.top
  ]

main :: IO ()
main = T.defaultMain allCheckedTests
