{-# LANGUAGE LiberalTypeSynonyms #-}

module Contextualise.Contextify where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar as DesugarS
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types as AST
import qualified Juvix.FrontendContextualise as Contextualize
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library
import qualified Juvix.Library.Parser.Internal as Internal
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "contextify tests:"
    [infixPlaceTest, sumConTest, sexpression]

sexpression :: T.TestTree
sexpression =
  T.testGroup
    "s-expression contextify tests:"
    [sumConTestS, defunTransfomrationWorks]

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
        <$> Parser.parse "let (+) = 3 declare infixl (+) 5"

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
        <$> Parser.parse "type bool = True | False"

-------------------------------------------------------------------------------
-- S Expression Tests
-------------------------------------------------------------------------------
sumConTestS :: T.TestTree
sumConTestS =
  T.testGroup
    "Sum Constructors are properly added:"
    [ T.testCase "Bool properly adds True" (test "True"),
      T.testCase "Bool properly adds False" (test "False")
    ]
  where
    test str = do
      Right (ctx, _) <-
        Contextualize.contextifyS (("Foo", desugared) :| [])
      ctx Context.!? str
        |> fmap Context.extractValue
        |> (T.@=? Just (Context.SumCon (Context.Sum Nothing "bool")))
    Right desugared =
      extract "type bool = True | False"

defunTransfomrationWorks :: T.TestTree
defunTransfomrationWorks =
  T.testCase "defun properly added" test
  where
    test = do
      Right (ctx, _) <-
        Contextualize.contextifyS (("Foo", desugared) :| [])
      let Just (Context.Def x) = ctx Context.!? "foo" >>| Context.extractValue
      [Context.defMTy x, Just (Context.defTerm x)] T.@=? [Just sig, Just function]
    Right desugared =
      extract "sig foo : int -> int let foo 1 = 1 let foo n = n * foo (pred n)"
    Right function =
      Sexp.parse "(((1) 1) ((n) (:infix * n (foo (:paren (pred n))))))"
    Right sig =
      Sexp.parse "(:infix -> int int)"

extract :: ByteString -> Either Internal.ParserError [Sexp.T]
extract s =
  Parser.parse s
    >>| DesugarS.op . fmap SexpTrans.transTopLevel . AST.extractTopLevel
