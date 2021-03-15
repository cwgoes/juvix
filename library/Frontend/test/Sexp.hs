module Sexp (top) where

import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as Trans
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (head)
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)

--------------------------------------------------------------------------------
-- Exported top level test
--------------------------------------------------------------------------------

-- | @top@ checks that every form has proper converted from the ML
-- syntax to the lisp syntax
top :: T.TestTree
top =
  T.testGroup
    "top level tests"
    [ topLevelLet,
      topLevelSig,
      topLevelType,
      topLevelDeclaration,
      topLevelModule,
      letTest,
      moduleTest,
      tupleTest,
      listTest,
      recordTest,
      doTest,
      lambdaTest,
      openTest,
      parenTest,
      blockTest,
      primitiveTest,
      condTest,
      caseTest
    ]

--------------------------------------------------------------------------------
-- Top Level Tests
--------------------------------------------------------------------------------

topLevelLet :: T.TestTree
topLevelLet =
  T.testGroup
    "defun/ top level let tests"
    [T.testCase "guard test" (guard T.@=? guardExpected)]
  where
    guard =
      Parser.parse
        "let foo x \
        \  | x == 2 = 3 + 4 + x \
        \  | else   = x + 2"
        |> singleEleErr
    guardExpected =
      Sexp.parse
        "(:defun foo (x) \
        \   (:cond \
        \    ((:infix == x 2) (:infix + 3 (:infix + 4 x))) \
        \    (else            (:infix + x 2))))"

topLevelSig :: T.TestTree
topLevelSig =
  T.testGroup
    "Sig tests"
    [ T.testCase "basic" (basic T.@=? basicExpected),
      T.testCase "usage" (usage T.@=? usageExpected)
    ]
  where
    basic =
      Parser.parse "sig foo : int -> int -> int" |> singleEleErr
    basicExpected =
      Sexp.parse "(:defsig foo (:infix -> int (:infix -> int int)))"
    usage =
      Parser.parse "sig foo 0 : int -> int -> int" |> singleEleErr
    -- may change in the future
    usageExpected =
      Sexp.parse "(:defsig foo (:usage 0 (:infix -> int (:infix -> int int))))"

topLevelType :: T.TestTree
topLevelType =
  T.testGroup
    "Types parser"
    [ T.testCase "record" (record T.@=? recordExpected),
      T.testCase "∑ Complex" (sumC T.@=? sumCExpected),
      T.testCase "signature" (sig T.@=? sigExpected)
    ]
  where
    record =
      Parser.parse
        "type foo x y z = \
        \ { y-axis : y \
        \ , x-axis : x \
        \ , z-axis : z }"
        |> singleEleErr
    recordExpected =
      Sexp.parse
        "(type foo (x y z) \
        \ (:record-d \
        \  y-axis y \
        \  x-axis x \
        \  z-axis z))"
    sumC =
      Parser.parse
        "type foo x y z = \
        \  | Foo {y-axis : y, x-axis : x, z-axis : z} \
        \  | Bar (var1 : x) (var2 : y) \
        \  | Car : hd : x -> cdr : foo y -> foo x y z"
        |> singleEleErr
    sumCExpected =
      Sexp.parse
        "(type foo (x y z) \
        \  (Foo (:record-d y-axis y x-axis x z-axis z)) \
        \  (Bar (:paren (:infix : var1 x)) \
        \       (:paren (:infix : var2 y))) \
        \  (Car (:arrow \
        \        (:infix : hd \
        \                (:infix -> x \
        \                       (:infix : cdr \
        \                       (:infix -> (foo y) (foo x y z))))))))"
    sig =
      Parser.parse "type foo x y z : typ -> typ -> typ = Foo a b c"
        |> singleEleErr
    sigExpected =
      Sexp.parse
        "(type (foo :type (:infix -> typ (:infix -> typ typ))) (x y z) \
        \   (Foo a b c))"

topLevelDeclaration :: T.TestTree
topLevelDeclaration =
  T.testGroup
    "declaration parser"
    [ T.testCase "infix" (basic T.@=? basicE)
    ]
  where
    basic = Parser.parse "declare infixl foo 3" |> singleEleErr
    basicE = Sexp.parse "(declare infixl foo 3)"

topLevelModule :: T.TestTree
topLevelModule =
  T.testGroup
    "module parser"
    [ T.testCase "basic" (basic T.@=? basicExpected),
      T.testCase "guard" (guard T.@=? guardExpected)
    ]
  where
    basic =
      Parser.parse
        "mod foo = \
        \   let bar = 3 \
        \   type zar = Boo \
        \ end"
        |> singleEleErr
    basicExpected =
      Sexp.parse
        "(:defmodule foo () \
        \  (:defun bar () 3) \
        \  (type zar () (Boo)))"
    guard =
      Parser.parse
        "mod foo x \
        \  | x == 3 = \
        \    let foo = 3 \
        \    let bar = 5 \
        \  | else = \
        \    let foo = 5 \
        \ end"
        |> singleEleErr
    guardExpected =
      Sexp.parse
        "(:defmodule foo (x) \
        \   (:cond ((:infix == x 3) \
        \           (:defun foo () 3) \
        \           (:defun bar () 5)) \
        \          (else \
        \           (:defun foo () 5))))"

--------------------------------------------------------------------------------
-- Expression Tests
--------------------------------------------------------------------------------

letTest :: T.TestTree
letTest =
  T.testGroup
    "let parser"
    [ T.testCase "basic" (basic T.@=? basicExpected),
      T.testCase "argument" (arguments T.@=? argumentsExpected)
    ]
  where
    basic =
      Parser.parse
        "let foo y = \
        \  let fi = 3 in \
        \  fi"
        |> singleEleErr
    basicExpected =
      Sexp.parse
        "(:defun foo (y) \
        \  (let fi () 3 \
        \    fi))"
    arguments =
      Parser.parse
        "let foo y = \
        \  let fi x = 3 + x in \
        \  fi y"
        |> singleEleErr
    argumentsExpected =
      Sexp.parse
        "(:defun foo (y) \
        \   (let fi (x) (:infix + 3 x) \
        \      (fi y)))"

moduleTest :: T.TestTree
moduleTest =
  T.testGroup
    "let parser"
    [T.testCase "basic" (basic T.@=? basicExpected)]
  where
    basic =
      Parser.parse
        "let foo = \
        \  mod Bar = \
        \    let bar = 3 \
        \     type zar = Boo \
        \  end in \
        \  Bar.bar"
        |> singleEleErr
    basicExpected =
      Sexp.parse
        "(:defun foo () \
        \   (:let-mod Bar () \
        \       ((:defun bar () 3) \
        \        (type zar () (Boo))) \
        \     Bar.bar))"

tupleTest :: T.TestTree
tupleTest =
  T.testGroup
    "tuple parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic = Parser.parse "let foo = (1,2,3)" |> singleEleErr
    basicE = Sexp.parse "(:defun foo () (:tuple 1 2 3))"

listTest :: T.TestTree
listTest =
  T.testGroup
    "list parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic = Parser.parse "let foo = [1,2,3,4]" |> singleEleErr
    basicE = Sexp.parse "(:defun foo () (:list 1 2 3 4))"

recordTest :: T.TestTree
recordTest =
  T.testGroup
    "record parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic = Parser.parse "let foo = {a, b = 2}" |> singleEleErr
    basicE = Sexp.parse "(:defun foo () (:record (a) (b 2)))"

doTest :: T.TestTree
doTest =
  T.testGroup
    "do parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse
        "let foo xs = \
        \  a <- xs; \
        \  more-comp; \
        \  pure a"
        |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo (xs) \
        \    (:do (%<- a xs) \
        \         more-comp \
        \         (pure a)))"

lambdaTest :: T.TestTree
lambdaTest =
  T.testGroup
    "lambda parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse
        "let foo y = \
        \  \\x -> x + y"
        |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo (y) \
        \   (:lambda (x) (:infix + x y)))"

openTest :: T.TestTree
openTest =
  T.testGroup
    "open parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse
        "let foo y = \
        \  open Prelude in x + y"
        |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo (y) \
        \   (:open-in Prelude (:infix + x y)))"

parenTest :: T.TestTree
parenTest =
  T.testGroup
    "paren parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse "let foo y = (y + 3) * 9" |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo (y) \
        \   (:infix * (:paren (:infix + y 3)) 9))"

blockTest :: T.TestTree
blockTest =
  T.testGroup
    "block parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse "let foo y = 3 * begin y + y end" |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo (y) \
        \   (:infix * 3 (:progn (:infix + y y))))"

primitiveTest :: T.TestTree
primitiveTest =
  T.testGroup
    "block parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse "let add = %Michelson.add" |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun add () \
        \   (:primitive Michelson.add))"

condTest :: T.TestTree
condTest =
  T.testGroup
    "cond parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse
        "let add x = \
        \ if | x == 3 = 1 \
        \    | x == 4 = 5 \
        \    | else   = 0"
        |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun add (x) \
        \   (:cond ((:infix == x 3) 1) \
        \          ((:infix == x 4) 5) \
        \          (else            0)))"

caseTest :: T.TestTree
caseTest =
  T.testGroup
    "cond parser"
    [T.testCase "basic" (basic T.@=? basicE)]
  where
    basic =
      Parser.parse
        "let foo = \
        \  case x of \
        \   | (A (B {a, b})) -> a + b \
        \   | A (B c) (C d)  -> c + d"
        |> singleEleErr
    basicE =
      Sexp.parse
        "(:defun foo () \
        \  (case x \
        \    ((A (B (:record (a) (b)))) (:infix + a b)) \
        \    ((A (B c) (C d))           (:infix + c d))))"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (Trans.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "imporper form"
