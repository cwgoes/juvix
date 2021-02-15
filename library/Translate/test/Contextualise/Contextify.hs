{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Contextify where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as AST
import qualified Juvix.FrontendContextualise as Contextualize
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "contextify tests:"
    [infixPlaceTest, sumConTest]

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

infixPlaceTest :: T.TestTree
infixPlaceTest =
  ( do
      Right (ctx, _) <-
        Contextualize.contextify (("Foo", desugared) :| [])
      ctx Context.!? "+"
        |> fmap ((\(Context.Def d) -> Context.defPrecedence d) . Context.extractValue)
        |> (T.@=? Just (Context.Pred Context.Left 5))
      |> T.testCase
        "infix properly adds precedence"
  )
  where
    Right desugared =
      Desugar.op . AST.extractTopLevel
        <$> Parser.parseOnly "let (+) = 3 declare infixl (+) 5"

sumConTest :: T.TestTree
sumConTest =
  T.testGroup
    "Sum Constructors are properly added:"
    [ T.testCase "Bool properly adds True" (test "True"),
      T.testCase "Bool properly adds False" (test "False")
    ]
  where
    test str = do
      Right (ctx, _) <-
        Contextualize.contextify (("Foo", desugared) :| [])
      ctx Context.!? str
        |> fmap Context.extractValue
        |> (T.@=? Just (Context.SumCon (Context.Sum Nothing "bool")))
    Right desugared =
      Desugar.op . AST.extractTopLevel
        <$> Parser.parseOnly "type bool = True | False"
