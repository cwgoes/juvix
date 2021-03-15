module Parser where

import Juvix.Frontend.Parser (parse)
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (Expression, TopLevel)
import qualified Juvix.Frontend.Types as AST
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSym
import Juvix.Library.Parser (Parser, ParserError)
import qualified Juvix.Library.Parser as J
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import Prelude (String, error)

allParserTests :: T.TestTree
allParserTests =
  T.testGroup
    "Parser Tests"
    [ many1FunctionsParser,
      sigTest1,
      sigTest2,
      fun1,
      fun2,
      sumTypeTest,
      superArrowCase,
      typeTest,
      moduleOpen,
      moduleOpen',
      typeNameNoUniverse,
      simpleNamedCon,
      matchMoreComplex,
      condTest1,
      record1,
      parens1,
      infixTests,
      -- pre-processor tests
      removeNoComment,
      removeNewLineBefore,
      removeSpaceBefore,
      nonassocTest,
      spacerSymb,
      vpsDashFrontFail,
      vpsDashMiddle,
      reservedInfix,
      letwordFail,
      reservedInfix,
      caseOfWords,
      questionMarktest,
      bangtest,
      removeNewLineNextToNewline
    ]

infixTests :: T.TestTree
infixTests =
  T.testGroup
    "Infix Tests"
    [ infxlTest,
      infxrTest,
      infixFail,
      qualifiedInfixTest,
      infxPlusTest,
      infixPlusFail
    ]

--------------------------------------------------------------------------------
-- Parser Checker
--------------------------------------------------------------------------------

test :: Either ParserError [Expression]
test =
  P.parse
    (P.many Parser.expressionSN)
    ""
    "let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
    \= 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
    \foo = 3 let foo = 3 let foo = 3 let foo = 3 "

shouldParseAs ::
  (Show a, Eq a) => T.TestName -> (ByteString -> Either ParserError a) -> ByteString -> a -> T.TestTree
shouldParseAs name parses x y =
  T.testGroup
    "Parse tests"
    [ T.testCase
        ("parse: " <> name <> " " <> show x <> " should parse to " <> show y)
        (y T.@=? either (panic . toS . P.errorBundlePretty) identity (parses x))
    ]

--------------------------------------------------------------------------------
-- Pre-processor test
--------------------------------------------------------------------------------
removeSpaceBefore :: T.TestTree
removeSpaceBefore =
  Parser.removeComments "let foo = 3 \n + \n -- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n \n 4")
    |> T.testCase "test remove comments: space before"

removeNewLineBefore :: T.TestTree
removeNewLineBefore =
  Parser.removeComments "let foo = 3 \n + \n-- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n\n 4")
    |> T.testCase "test remove comments: new line before"

removeNewLineNextToNewline :: T.TestTree
removeNewLineNextToNewline =
  Parser.removeComments "\n -- -- ffooo \n--\n -- -- \n let foo xs = 3 -- bar"
    |> (T.@=? "\n \n\n \n let foo xs = 3 ")
    |> T.testCase "test remove comments: remove new line next to newline"

-- TODO :: use quick check!

removeNoComment :: T.TestTree
removeNoComment =
  let str = "let foo = 3 \n + \n \n 4"
   in Parser.removeComments str
        |> (T.@=? str)
        |> T.testCase "test remove comments where there are none"

--------------------------------------------------------------------------------
-- Parse Many at once
--------------------------------------------------------------------------------
many1FunctionsParser :: T.TestTree
many1FunctionsParser =
  shouldParseAs
    "many1FunctionsParser"
    (P.parse (P.some Parser.topLevelSN) "")
    ( ""
        <> "let foo a b c = (+) (a + b) c\n"
        <> "let bah = foo 1 2 3\n"
        <> "let nah \n"
        <> "  | bah == 5 = 7 \n"
        <> "  | else     = 11"
        <> "let test = \n"
        <> "  let check = nah in \n"
        <> "  case check of \n"
        <> "  | seven -> 11 \n"
        <> "  | eleven -> 7 \n"
        <> "  | f  -> open Fails in \n"
        <> "          print failed; \n"
        <> "          fail"
    )
    [ ( AST.Inf (AST.Name "a") "+" (AST.Name "b")
          |> AST.Infix
          |> AST.Parened
      )
        :| [AST.Name "c"]
        |> AST.App (AST.Name "+")
        |> AST.Application
        |> AST.Body
        |> AST.Like
          "foo"
          [ AST.ConcreteA (AST.MatchLogic (AST.MatchName "a") Nothing),
            AST.ConcreteA (AST.MatchLogic (AST.MatchName "b") Nothing),
            AST.ConcreteA (AST.MatchLogic (AST.MatchName "c") Nothing)
          ]
        |> AST.Func
        |> AST.Function,
      --
      ( AST.Constant (AST.Number (AST.Integer' 1))
          :| [ AST.Constant (AST.Number (AST.Integer' 2)),
               AST.Constant (AST.Number (AST.Integer' 3))
             ]
      )
        |> AST.App (AST.Name "foo")
        |> AST.Application
        |> AST.Body
        |> AST.Like "bah" []
        |> AST.Func
        |> AST.Function,
      --
      ( AST.CondExpression
          { condLogicPred =
              AST.Integer' 5
                |> AST.Number
                |> AST.Constant
                |> AST.Inf (AST.Name "bah") "=="
                |> AST.Infix,
            condLogicBody =
              AST.Constant (AST.Number (AST.Integer' 7))
          }
          :| [ AST.Integer' 11
                 |> AST.Number
                 |> AST.Constant
                 |> AST.CondExpression (AST.Name "else")
             ]
      )
        |> AST.C
        |> AST.Guard
        |> AST.Like "nah" []
        |> AST.Func
        |> AST.Function,
      --

      AST.Let''
        { letBindings =
            AST.Name "nah" |> AST.Body |> AST.Like "check" [],
          letBody =
            ( AST.MatchL
                { matchLPattern =
                    AST.MatchLogic (AST.MatchName "seven") Nothing,
                  matchLBody =
                    AST.Constant (AST.Number (AST.Integer' 11))
                }
                :| [ AST.Integer' 7
                       |> AST.Number
                       |> AST.Constant
                       |> AST.MatchL (AST.MatchLogic (AST.MatchName "eleven") Nothing),
                     --

                     (AST.Name "failed" :| [])
                       |> AST.App (AST.Name "print")
                       |> AST.Application
                       |> AST.DoBody Nothing
                       |> (:| [AST.DoBody Nothing (AST.Name "fail")])
                       |> AST.Do''
                       |> AST.Do
                       |> AST.OpenExpress "Fails"
                       |> AST.OpenExpr
                       |> AST.MatchL (AST.MatchLogic (AST.MatchName "f") Nothing)
                   ]
            )
              |> AST.Match'' (AST.Name "check")
              |> AST.Match
        }
        |> AST.Let
        |> AST.Body
        |> AST.Like "test" []
        |> AST.Func
        |> AST.Function
    ]

--------------------------------------------------------------------------------
-- Sig Test
--------------------------------------------------------------------------------

sigTest1 :: T.TestTree
sigTest1 =
  shouldParseAs
    "sigTest1"
    Parser.parse
    "sig foo 0 : Int -> Int"
    $ AST.NoHeader
      [ AST.Name "Int"
          |> AST.Inf (AST.Name "Int") "->"
          |> AST.Infix
          |> flip (AST.Sig "foo" (Just (AST.Constant (AST.Number (AST.Integer' 0))))) []
          |> AST.Signature
      ]

sigTest2 :: T.TestTree
sigTest2 =
  shouldParseAs
    "sigTest2"
    Parser.parse
    "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"
    $ AST.NoHeader
      [ AST.Integer' 1
          |> AST.Number
          |> AST.Constant
          |> AST.Inf (AST.Name "i") ">"
          |> AST.Infix
          |> AST.TypeRefine (AST.Name "Int")
          |> AST.RefinedE
          |> AST.Inf
            ( AST.Integer' 0
                |> AST.Number
                |> AST.Constant
                |> AST.Inf (AST.Name "i") ">"
                |> AST.Infix
                |> AST.TypeRefine (AST.Name "Int")
                |> AST.RefinedE
            )
            "->"
          |> AST.Infix
          |> AST.Inf (AST.Name "i") ":"
          |> AST.Infix
          |> flip (AST.Sig "foo" (Just (AST.Constant (AST.Number (AST.Integer' 0))))) []
          |> AST.Signature
      ]

-- --------------------------------------------------------------------------------
-- -- Function Testing
-- --------------------------------------------------------------------------------

fun1 :: T.TestTree
fun1 =
  shouldParseAs
    "fun1"
    Parser.parse
    "let f foo@(A b c d) = 3"
    $ AST.NoHeader
      [ AST.Integer' 3
          |> AST.Number
          |> AST.Constant
          |> AST.Body
          |> AST.Like
            "f"
            [ [ AST.MatchLogic (AST.MatchName "b") Nothing,
                AST.MatchLogic (AST.MatchName "c") Nothing,
                AST.MatchLogic (AST.MatchName "d") Nothing
              ]
                |> AST.MatchCon "A"
                |> flip AST.MatchLogic (Just "foo")
                |> AST.ConcreteA
            ]
          |> AST.Func
          |> AST.Function
      ]

fun2 :: T.TestTree
fun2 =
  shouldParseAs
    "fun2"
    Parser.parse
    "let f foo | foo = 2 | else = 3"
    $ AST.NoHeader
      [ ( AST.Integer' 2
            |> AST.Number
            |> AST.Constant
            |> AST.CondExpression (AST.Name "foo")
        )
          :| [ AST.Integer' 3
                 |> AST.Number
                 |> AST.Constant
                 |> AST.CondExpression (AST.Name "else")
             ]
          |> AST.C
          |> AST.Guard
          |> AST.Like
            "f"
            [AST.ConcreteA (AST.MatchLogic (AST.MatchName "foo") Nothing)]
          |> AST.Func
          |> AST.Function
      ]

--------------------------------------------------------------------------------
-- Type tests
--------------------------------------------------------------------------------

--------------------------------------------------
-- adt testing
--------------------------------------------------

sumTypeTest :: T.TestTree
sumTypeTest =
  shouldParseAs
    "sumTypeTest"
    Parser.parse
    ( "type Foo a b c = | A : b : a -> b -> c \n"
        <> "            | B : d -> Foo \n"
        <> "            | C { a : Int, #b : Int } \n"
        <> "            | D { a : Int, #b : Int } : Foo Int (Fooy -> Nada)"
    )
    $ AST.NoHeader
      [ ( AST.Name "c"
            |> AST.Inf (AST.Name "b") "->"
            |> AST.Infix
            |> AST.Inf (AST.Name "a") "->"
            |> AST.Infix
            |> AST.Inf (AST.Name "b") ":"
            |> AST.Infix
            |> AST.Arrow
            |> Just
            |> AST.S "A"
        )
          :| [ AST.Name "Foo"
                 |> AST.Inf (AST.Name "d") "->"
                 |> AST.Infix
                 |> AST.Arrow
                 |> Just
                 |> AST.S "B",
               --
               AST.NameType' (AST.Name "Int") (AST.Concrete "a")
                 :| [AST.NameType' (AST.Name "Int") (AST.Implicit "b")]
                 |> flip AST.Record'' Nothing
                 |> AST.Record
                 |> Just
                 |> AST.S "C",
               --
               (AST.Name "Int")
                 :| [ (AST.Name "Nada")
                        |> AST.Inf
                          (AST.Name "Fooy")
                          "->"
                        |> AST.Infix
                        |> AST.Parened
                    ]
                 |> AST.App (AST.Name "Foo")
                 |> AST.Application
                 |> Just
                 |> AST.Record''
                   ( AST.NameType' (AST.Name "Int") (AST.Concrete "a")
                       :| [AST.NameType' (AST.Name "Int") (AST.Implicit "b")]
                   )
                 |> AST.Record
                 |> Just
                 |> AST.S "D"
             ]
          |> AST.Sum
          |> AST.NonArrowed
          |> AST.Typ Nothing "Foo" ["a", "b", "c"]
          |> AST.Type
      ]

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase :: T.TestTree
superArrowCase =
  AST.Name "foo"
    |> AST.Inf (AST.Name "HAHAHHA") "->"
    |> AST.Infix
    |> AST.Parened
    |> AST.Inf
      ( AST.App
          (AST.Name "Bah")
          (AST.Name "a" :| [(AST.Name "c")])
          |> AST.Application
      )
      "-o"
    |> AST.Infix
    |> AST.Inf (AST.Name "a") ":"
    |> AST.Infix
    |> AST.Inf
      ( AST.App
          (AST.Name "Foo")
          (AST.Name "a" :| [(AST.Name "b")])
          |> AST.Application
      )
      "->"
    |> AST.Infix
    |> AST.Inf
      ( AST.Name "Foo"
          |> AST.Inf (AST.Name "B") "-o"
          |> AST.Infix
          |> AST.Inf (AST.Name "c") ":"
          |> AST.Infix
          |> AST.Inf (AST.Name "Bah") "->"
          |> AST.Infix
          |> AST.Inf (AST.Name "b") ":"
          |> AST.Infix
          |> AST.Parened
      )
      "->"
    |> AST.Infix
    |> shouldParseAs
      "superArrowCase"
      (P.parse Parser.expression "")
      "( b : Bah ->  c : B -o Foo) -> Foo a b -> a : Bah a c -o ( HAHAHHA -> foo )"

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest :: T.TestTree
typeTest =
  shouldParseAs
    "typeTest"
    Parser.parse
    "type Foo a b c d = | Foo nah bah sad"
    $ AST.NoHeader
      [ [ AST.Name "nah",
          AST.Name "bah",
          AST.Name "sad"
        ]
          |> AST.ADTLike
          |> Just
          |> AST.S "Foo"
          |> (:| [])
          |> AST.Sum
          |> AST.NonArrowed
          |> AST.Typ Nothing "Foo" ["a", "b", "c", "d"]
          |> AST.Type
      ]

--------------------------------------------------------------------------------
-- Modules test
--------------------------------------------------------------------------------

moduleOpen :: T.TestTree
moduleOpen =
  shouldParseAs
    "moduleOpen"
    Parser.parse
    ( ""
        <> "mod Foo Int = \n"
        <> "  let T = Int.t \n"
        <> "  sig bah : T -> T \n"
        <> "  let bah t = Int.(t + 3) \n"
        <> "end"
    )
    $ AST.NoHeader
      [ ( AST.Name "Int.t"
            |> AST.Body
            |> AST.Like "T" []
            |> AST.Func
            |> AST.Function
        )
          :| [ AST.Inf
                 (AST.Name "T")
                 "->"
                 (AST.Name "T")
                 |> AST.Infix
                 |> flip (AST.Sig "bah" Nothing) []
                 |> AST.Signature,
               --
               AST.Inf
                 (AST.Name "t")
                 "+"
                 (AST.Constant (AST.Number (AST.Integer' 3)))
                 |> AST.Infix
                 |> AST.OpenExpress "Int"
                 |> AST.OpenExpr
                 |> AST.Body
                 |> AST.Like
                   "bah"
                   [AST.ConcreteA (AST.MatchLogic (AST.MatchName "t") Nothing)]
                 |> AST.Func
                 |> AST.Function
             ]
          |> AST.Body
          |> AST.Like
            "Foo"
            [ AST.MatchLogic (AST.MatchCon "Int" []) Nothing
                |> AST.ConcreteA
            ]
          |> AST.Mod
          |> AST.Module
      ]

moduleOpen' :: T.TestTree
moduleOpen' =
  shouldParseAs
    "moduleOpen'"
    Parser.parse
    ( ""
        <> "mod Bah M = \n"
        <> "  open M"
        <> "  sig bah : Rec \n"
        <> "  let bah t = \n"
        <> "     { a = t + 3"
        <> "     , b = expr M.N.t}"
        <> "end"
    )
    $ AST.NoHeader
      [ AST.ModuleOpen (AST.Open "M")
          :| [ AST.Sig "bah" Nothing (AST.Name "Rec") []
                 |> AST.Signature,
               AST.NonPunned
                 "a"
                 ( AST.Inf
                     (AST.Name "t")
                     "+"
                     (AST.Constant (AST.Number (AST.Integer' 3)))
                     |> AST.Infix
                 )
                 :| [ AST.Name "M.N.t" :| []
                        |> AST.App (AST.Name "expr")
                        |> AST.Application
                        |> AST.NonPunned "b"
                    ]
                   |> AST.ExpressionRecord
                   |> AST.ExpRecord
                   |> AST.Body
                   |> AST.Like
                     "bah"
                     [AST.ConcreteA (AST.MatchLogic (AST.MatchName "t") Nothing)]
                   |> AST.Func
                   |> AST.Function
             ]
          |> AST.Body
          |> AST.Like
            "Bah"
            -- this shouldn't be a matchCon but a match argument
            [ AST.MatchLogic (AST.MatchCon "M" []) Nothing
                |> AST.ConcreteA
            ]
          |> AST.Mod
          |> AST.Module
      ]

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse :: T.TestTree
typeNameNoUniverse =
  AST.Name "a"
    :| [ AST.Name "b",
         AST.Name "c",
         AST.Name "d"
           |> AST.Inf (AST.Name "b") "-o"
           |> AST.Infix
           |> AST.Parened,
         AST.Name "a",
         AST.Name "c",
         AST.Name "u"
       ]
    |> AST.App (AST.Name ("Foo" :| []))
    |> AST.Application
    |> shouldParseAs
      "typeNameNoUniverse"
      (P.parse Parser.expression "")
      "Foo a b c (b -o d) a c u"

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon :: T.TestTree
simpleNamedCon =
  [ AST.MatchLogic (AST.MatchName "a") Nothing,
    AST.MatchLogic (AST.MatchName "b") Nothing,
    AST.MatchLogic (AST.MatchName "c") Nothing
  ]
    |> AST.MatchCon "Hi"
    |> flip AST.MatchLogic (Just "foo")
    |> shouldParseAs
      "simpleNamedCon"
      (P.parse Parser.matchLogic "")
      "foo@( Hi a b c )"

matchMoreComplex :: T.TestTree
matchMoreComplex =
  [ Nothing
      |> AST.MatchLogic (AST.MatchName "nah")
      |> AST.NonPunned "a"
      |> (:| [AST.Punned "f"])
      |> AST.MatchRecord
      |> flip AST.MatchLogic (Just "nah"),
    --
    AST.MatchLogic (AST.MatchName "b") Nothing,
    --
    AST.MatchLogic (AST.MatchConst (AST.Number (AST.Integer' 5))) Nothing
  ]
    |> AST.MatchCon "Hi"
    |> flip AST.MatchLogic (Just "foo")
    |> shouldParseAs
      "matchMoreComplex"
      (P.parse Parser.matchLogic "")
      "foo@( Hi nah@{ a = nah , f } b 5 )"

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 :: T.TestTree
condTest1 =
  AST.CondExpression
    { condLogicPred = AST.Name "foo",
      condLogicBody = AST.Name "a"
    }
    :| [ AST.CondExpression
           { condLogicPred = AST.Name "else",
             condLogicBody = AST.Name "b"
           }
       ]
    |> AST.C
    |> shouldParseAs
      "condTest1"
      (P.parse Parser.cond "")
      ( ""
          <> "if  | foo  = a\n"
          <> "    | else = b "
      )

--------------------------------------------------
-- Record
--------------------------------------------------

record1 :: T.TestTree
record1 =
  AST.Punned "a"
    :| [ AST.Inf
           { infixLeft = AST.Constant (AST.Number (AST.Integer' 3)),
             infixOp = "+",
             infixRight = AST.Constant (AST.Number (AST.Integer' 5))
           }
           |> AST.Infix
           |> AST.NonPunned "b"
       ]
    |> AST.ExpressionRecord
    |> AST.ExpRecord
    |> shouldParseAs
      "record1"
      (P.parse Parser.expression "")
      "{a, b = 3+5}"

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 :: T.TestTree
parens1 =
  AST.Punned "a"
    :| [ AST.Integer' 5
           |> AST.Number
           |> AST.Constant
           |> AST.Inf (AST.Constant (AST.Number (AST.Integer' 3))) "+"
           |> AST.Infix
           |> AST.NonPunned "b"
       ]
    |> AST.ExpressionRecord
    |> AST.ExpRecord
    |> AST.Parened
    |> AST.Parened
    |> AST.Parened
    |> AST.Parened
    |> shouldParseAs
      "parens1"
      (P.parse Parser.expression "")
      "(       ( (({a, b = 3+5}))))"

--------------------------------------------------
-- Infix Tests
--------------------------------------------------

nonassocTest :: T.TestTree
nonassocTest =
  shouldParseAs
    "declare infix foo 5"
    Parser.parse
    "declare infix foo 5"
    $ AST.NoHeader
      [AST.Declaration (AST.Infixivity (AST.NonAssoc "foo" 5))]

infxrTest :: T.TestTree
infxrTest =
  shouldParseAs
    "declare infixr foo 5"
    Parser.parse
    "declare infixr foo 5"
    $ AST.NoHeader
      [AST.Declaration (AST.Infixivity (AST.AssocR "foo" 5))]

infxlTest :: T.TestTree
infxlTest =
  shouldParseAs
    "declare infixl foo 5"
    Parser.parse
    "declare infixl foo 5"
    $ AST.NoHeader
      [AST.Declaration (AST.Infixivity (AST.AssocL "foo" 5))]

infxPlusTest :: T.TestTree
infxPlusTest =
  shouldParseAs
    "declare infixl (+) 5"
    Parser.parse
    "declare infixl (+) 5"
    $ AST.NoHeader
      [AST.Declaration (AST.Infixivity (AST.AssocL "+" 5))]

infixPlusFail :: T.TestTree
infixPlusFail =
  T.testCase
    "parse: declare infixl + 5 should fail"
    (isLeft (Parser.parse "declare infixl + 5") T.@=? True)

infixFail :: T.TestTree
infixFail =
  T.testCase
    "parse: declare infixl foo.o 5 should fail"
    (isLeft (Parser.parse "declare infixl foo.o 5") T.@=? True)

qualifiedInfixTest :: T.TestTree
qualifiedInfixTest =
  shouldParseAs
    "infix qualified"
    Parser.parse
    "let fi = nat TopLevel.Prelude.-> TopLevel.int"
    $ AST.NoHeader
      [ AST.Function
          $ AST.Func
          $ AST.Like "fi" []
          $ AST.Body
          $ AST.Infix
          $ AST.Inf (AST.Name ("nat" :| [])) ("TopLevel" :| ["Prelude", "->"])
          $ AST.Name ("TopLevel" :| ["int"])
      ]

--------------------------------------------------
-- reserved word tests
--------------------------------------------------

letwordFail :: T.TestTree
letwordFail =
  T.testCase
    "parse: letfad = 3 should fail"
    (isLeft (Parser.parse "letfad = 3") T.@=? True)

reservedInfix :: T.TestTree
reservedInfix =
  shouldParseAs
    "reserved then infix"
    Parser.parse
    "let(+) = %Michelson.plus"
    $ AST.NoHeader
      [ "Michelson.plus"
          |> AST.Prim
          |> AST.Primitive
          |> AST.Body
          |> AST.Like "+" []
          |> AST.Func
          |> AST.Function
      ]

caseOfWords :: T.TestTree
caseOfWords =
  shouldParseAs
    "caseOfWords"
    Parser.parse
    "let foo = case x-of (of-x)of | x -> y"
    $ AST.NoHeader
      [ "y"
          |> AST.Name
          |> AST.MatchL (AST.MatchLogic (AST.MatchName "x") Nothing)
          |> (:| [])
          |> AST.Match''
            ( "of-x"
                |> AST.Name
                |> AST.Parened
                |> (:| [])
                |> AST.App (AST.Name "x-of")
                |> AST.Application
            )
          |> AST.Match
          |> AST.Body
          |> AST.Like "foo" []
          |> AST.Func
          |> AST.Function
      ]

--------------------------------------------------------------------------------
-- Spacer tests
--------------------------------------------------------------------------------

spacerSymb :: T.TestTree
spacerSymb =
  let res =
        case P.parse (J.spacer Parser.prefixSymbol) "" "Foo   f" of
          Right f -> f == "Foo" -- && s == "Foo"
          _ -> False
   in T.testCase "symbol parser test: Foo f" (res T.@=? True)

--------------------------------------------------------------------------------
-- validPrefixSymbols
--------------------------------------------------------------------------------

vpsDashFrontFail :: T.TestTree
vpsDashFrontFail =
  T.testCase
    "-Foo is not a valid prefix symbol"
    (isLeft (P.parse Parser.prefixSymbol "" "-Foo") T.@=? True)

vpsDashMiddle :: T.TestTree
vpsDashMiddle =
  T.testCase
    "Foo-Foo is a valid prefix symbol"
    (isRight (P.parse Parser.prefixSymbol "" "Foo-Foo") T.@=? True)

questionMarktest :: T.TestTree
questionMarktest =
  T.testCase
    "foo? is a valid prefix symbol"
    (P.parse Parser.prefixSymbol "" "foo?" T.@=? Right "foo?")

bangtest :: T.TestTree
bangtest =
  T.testCase
    "foo! is a valid prefix symbol"
    (P.parse Parser.prefixSymbol "" "foo!" T.@=? Right "foo!")

--------------------------------------------------------------------------------
-- Examples for testing
--------------------------------------------------------------------------------

contractTest :: Either ParserError [TopLevel]
contractTest =
  P.parse
    (P.many Parser.topLevelSN)
    ""
    ( ""
        <> "mod Token = "
        <> "  let Address = s : String.T {String.length s == 36} \n"
        <> "\n"
        <> "  type Storage = { \n"
        <> "    total-supply : Nat.T, \n"
        <> "    accounts     : Accounts.T { Accounts.measure-value == total-supply } \n"
        <> "  }"
        <> "  sig empty-storage : Storage \n"
        <> "  let empty-storage = { \n"
        <> "    total-supply = 0, \n"
        <> "    accounts     = Accounts.empty, \n"
        <> "  } \n"
        <> " \n"
        <> "  type T = { \n"
        <> "    storage : Storage, \n"
        <> "    version : Nat.T, \n"
        <> "    name    : String.T, \n"
        <> "    symbol  : Char.T, \n"
        <> "    owner   : Address, \n"
        <> "  } \n"
        <> "end"
        <> " \n"
        <> "mod Transaction = \n"
        <> "  type Transfer = { \n"
        <> "    from-account : Token.Address, \n"
        <> "    to-account   : Token.Address, \n"
        <> "    ammount      : Nat.T, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Mint = { \n"
        <> "    mint-amount     : Nat.T, \n"
        <> "    mint-to-account : Token.Address, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Burn = { \n"
        <> "    burn-amount       : Nat.T, \n"
        <> "    burn-from-account : Token.Address, \n"
        <> "  } \n"
        <> " \n"
        <> "  type Data = \n"
        <> "    | Transfer : Transfer -> Data \n"
        <> "    | Mint     : Mint     -> Data \n"
        <> "    | Burn     : Burn     -> Data \n"
        <> " \n"
        <> "  type T = { \n"
        <> "    data               : Data, \n"
        <> "    authorized-account : Token.Address, \n"
        <> "  } \n"
        <> "end \n"
        <> " \n"
        <> "sig has-n : Accounts.T -> Token.Address -> Nat -> Bool \n"
        <> "let has-n accounts add to-transfer = \n"
        <> "  case Accounts.select accounts add of \n"
        <> "  | Just n  -> to-transfer <= n \n"
        <> "  | Nothing -> False \n"
        <> " \n"
        <> " \n"
        <> "sig account-sub : acc : Accounts.T \n"
        <> "               -> add : Token.Address \n"
        <> "               -> num : Nat.T {has-n acc add num} \n"
        <> "               -> Accounts.T \n"
        <> "let account-sub accounts add number = \n"
        <> "  case Accounts.select accounts add of \n"
        <> "  | Just balance -> \n"
        <> "     Accounts.put accounts add (balance - number) \n"
        <> " \n"
        <> "sig account-add : Accounts.T -> Token.Address -> Nat.T -> Accounts.T \n"
        <> "let account-add accounts add number = \n"
        <> "  Accounts.update accounts ((+) number) add \n"
        <> " \n"
        <> " \n"
        <> " \n"
        <> "sig transfer-stor : stor  : Token.Storage \n"
        <> "                 -> from  : Token.Address \n"
        <> "                 -> to    : Token.Address \n"
        <> "                 -> num   : Nat.T {has-n stor.accounts from num} \n"
        <> "                 -> Token.Storage \n"
        <> "let transfer-stor stor add_from add_to num = \n"
        <> "  let new-acc = account-add (account-sub stor.accounts add_from) add-to num in \n"
        <> "  { total-supply = stor.total-supply \n"
        <> "  , accounts     = new-acc \n"
        <> "  } \n"
        <> "mod Validation = \n"
        <> "  let T = Token.T -> Transaction.T -> Bool \n"
        <> " \n"
        <> "  let mint token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Mint -> \n"
        <> "      token.owner == tx-tx-authorized-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let transfer token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Transfer {from-account, amount} -> \n"
        <> "      has-n token.storage.accounts from-account amount \n"
        <> "      && tx.authroized-account == from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let Burn token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Burn {burn-from-account, burn-ammount} -> \n"
        <> "      has-n token.storage.accounts burn-from-account burn-amount \n"
        <> "      && tx.authroized-account == burn-from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> "end \n"
        <> " \n"
        <> "  type Error \n"
        <> "    = NotEnoughFunds \n"
        <> "    | NotSameAccount \n"
        <> "    | NotOwnerToken  \n"
        <> "    | NotEnoughTokens \n"
        <> " \n"
        <> "  sig exec : Token.T -> Transaction.T -> Either.T Error Token.T \n"
        <> "  let exec token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transfer _ -> \n"
        <> "      if | Validation.transfer token tx = Right (transfer token tx) \n"
        <> "         | else                         = Left NotEnoughFunds \n"
        <> "    | Mint _ -> \n"
        <> "      if | Validation.mint token tx = Right (mint token tx) \n"
        <> "         | else                     = Left NotEnoughFunds \n"
    )
