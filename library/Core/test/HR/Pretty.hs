{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module HR.Pretty
  (top)
where

import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Juvix.Library.PrettyPrint as PP
import Juvix.Core.HR as HR
import Juvix.Library.Usage
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import Data.String (String)
import qualified Text.Show

top :: T.TestTree
top =
  -- TODO test syntax highlighting stuff
  T.testGroup "HR pretty printing" [
    starTests,
    -- TODO pp for michelson primitives
    bindTests
  ]

starTests :: T.TestTree
starTests =
  T.testGroup "Star" [
    T.testCase "* wide" $
      prettyAt 1000 (Star 0) @?= "* 0",
    T.testCase "* tiny" $
      prettyAt 1 (Star 0) @?= "*\n0"
  ]

bindTests :: T.TestTree
bindTests =
  T.testGroup "Pi" [
    T.testCase "xAB wide" $
      prettyAt 1000 xAB @?=
      "Π (ω | x : A) → B x",
    T.testCase "xAB narrow" $
      prettyAt 15 xAB @?=
      "Π (ω | x : A) →\n\
      \  B x",
    T.testCase "xAyBC wide" $
      prettyAt 1000 xAyBC @?=
      "Π (ω | x : A) → Π (ω | y : B) → C x y",
    T.testCase "xAyBC narrow" $
      prettyAt 20 xAyBC @?=
      "Π (ω | x : A) →\n\
      \Π (ω | y : B) →\n\
      \  C x y",
    T.testCase "xAyBC' wide" $
      prettyAt 1000 xAyBC' @?=
      "Π (ω | x : A) → Σ (ω | y : B) → C x y",
    T.testCase "xAyBC' narrow" $
      prettyAt 20 xAyBC' @?=
      "Π (ω | x : A) →\n\
      \Σ (ω | y : B) →\n\
      \  C x y"
  ]
  where
    xAB =
      Pi Omega ["x"] (Elim (Var ["A"])) $
      Elim (App (Var ["B"]) (Elim (Var ["x"])))
    xAyBC =
      Pi Omega ["x"] (Elim (Var ["A"])) $
      Pi Omega ["y"] (Elim (Var ["B"])) $
      Elim (App (App (Var ["C"]) (Elim (Var ["x"]))) (Elim (Var ["y"])))
    xAyBC' =
      Pi Omega ["x"] (Elim (Var ["A"])) $
      Sig Omega ["y"] (Elim (Var ["B"])) $
      Elim (App (App (Var ["C"]) (Elim (Var ["x"]))) (Elim (Var ["y"])))


-- present errors more legibly: don't escape characters and instead put the
-- string on its own line, indented
newtype S = S String deriving Eq
instance Show S where
  show (S s) = "「\n  " ++ indent s ++ "\n」" where
    indent = concatMap \case '\n' -> "\n  "; c -> [c]

infix 1 @?=
(@?=) :: String -> String -> T.Assertion
a @?= b = S a T.@?= S b

prettyAt :: (a ~ tm HR.T Nat.Ty Nat.Val, PP.PrettySyntax a) => Int -> a -> String
prettyAt n x =
  PP.renderWith (PP.defaultOptions {PP.optsPageWidth = n}) $
  PP.prettyPrec0 x
