{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pretty (top) where

import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Test.Tasty.HUnit ((@?=))
import Data.String (String)


top :: T.TestTree
top = T.testGroup "pretty printer tests" [testHangsWith, testHangs]

testHangsWith :: T.TestTree
testHangsWith =
  T.testGroup "hangsWith" [
    T.testCase "wide" $
      renderWide hcw @?= "hello*cool*world",
    T.testCase "narrow" $
      renderNarrow hcw @?= "hello\n  cool\n  world"
  ]
  where hcw = PP.hangsWith "*" 2 "hello" ["cool", "world"]

testHangs :: T.TestTree
testHangs =
  T.testGroup "hangs" [
    T.testCase "wide" $
      renderWide hcw @?= "hello cool world",
    T.testCase "narrow" $
      renderNarrow hcw @?= "hello\n  cool\n  world"
  ]
  where hcw = PP.hangs 2 "hello" ["cool", "world"]


type Doc = PP.Doc ()

renderWidth :: Int -> Doc -> String
renderWidth n =
  PP.renderWith $ PP.defaultOptions {PP.optsPageWidth = n}

renderNarrow, renderWide :: Doc -> String
renderNarrow = renderWidth 5
renderWide = renderWidth 5000
