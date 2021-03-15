-- |
-- Temporary conversion from the Sexpression syntax to the ML syntax
module Conversion.ML where

-- for local testing as development only

-- import qualified Juvix.Conversion.ML as ML
-- import qualified Juvix.Desugar as Desugar
-- import qualified Juvix.Frontend.Parser as Parser
-- import qualified Juvix.Frontend.Sexp as SexpTrans
-- import qualified Juvix.Frontend.Types.Base as Frontend
-- import qualified Juvix.FrontendDesugar as MLPasses
-- import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
-- import Juvix.Library hiding (product, sum)
-- import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T

-- import qualified Test.Tasty.HUnit as T
-- import Prelude (error)

top :: T.TestTree
top =
  T.testGroup
    "passes agree upon results:"
    []
-- [functionTests, expressionTests, typeTests, openTests]

-- shouldBeTheSame :: T.TestName -> ByteString -> T.TestTree
-- shouldBeTheSame name str =
--   T.testCase
--     ("passes agree upon: " <> name)
--     (parseDesugarML str T.@=? fmap ML.op (parseDesugarSexp str))

-- ------------------------------------------------------------
-- -- Tests
-- ------------------------------------------------------------

-- functionTests :: T.TestTree
-- functionTests =
--   T.testGroup
--     "function tests"
--     [ shouldBeTheSame "basic" "let foo = 3",
--       shouldBeTheSame "guards" "let foo x | x == 3 = 2 | else = 5",
--       shouldBeTheSame "multiple" "let foo x = 3 let foo y = 5 let bar = 2",
--       shouldBeTheSame "implicit" "let f #foo = fooo",
--       shouldBeTheSame "sig" "sig foo : int -> int let foo x = 3 let foo y = 5"
--     ]

-- openTests :: T.TestTree
-- openTests =
--   T.testGroup
--     "function tests"
--     [shouldBeTheSame "basic" "open Michelosn.Alias"]

-- typeTests :: T.TestTree
-- typeTests =
--   T.testGroup
--     "type tests"
--     [ shouldBeTheSame "lambda" "let f = \\x y -> x y z",
--       shouldBeTheSame "records" "type foo = {a : int, b : string}",
--       shouldBeTheSame "singleSum" "type foo = | Foo {a : int, b : string}",
--       shouldBeTheSame "singleSum" "type foo = | Foo Int String",
--       shouldBeTheSame "extra-info" "type foo : Type.t = Foo Int String | Bar Int",
--       shouldBeTheSame "multipleSums" "type foo = Foo Int String | Bar Int",
--       shouldBeTheSame
--         "record-extra-info"
--         "type foo : Type.t = {a : int, b : string} : int -> string -> foo"
--     ]

-- expressionTests :: T.TestTree
-- expressionTests =
--   T.testGroup
--     "expression tests"
--     [ letTests,
--       constantTests,
--       shouldBeTheSame "case" "let f = case x of | Cons a b -> True | Nil -> False",
--       shouldBeTheSame "refinement" "let f = n {n == 3}",
--       shouldBeTheSame "primitive" "let f = %Michelson.bar let g = %Michelson.Back.bar",
--       shouldBeTheSame "delcaim" "let f = declare infixl (+) 5 in 2 + 4",
--       shouldBeTheSame "open-in" "let f = open Foo in bar",
--       shouldBeTheSame "open.in" "let f = Foo.(bar)",
--       shouldBeTheSame "records" "let f = {a,b = 3}",
--       shouldBeTheSame "parens" "let f = (((3)))",
--       shouldBeTheSame "tuples" "let f = (1,2,3,4,5,1,1,az)",
--       shouldBeTheSame "infix" "let f = 3 + 4 * 3 / 12",
--       shouldBeTheSame "progn" "let f = begin f begin 3 end 4 end",
--       shouldBeTheSame "list" "let f = [1,2,3,4,5,1,1,az]"
--     ]

-- letTests :: T.TestTree
-- letTests =
--   T.testGroup
--     "let tests"
--     [ shouldBeTheSame "basic" "let f = let foo = 3 in foo",
--       shouldBeTheSame "multiple" "let f = let foo x = 3 in let foo y = 4 in foo",
--       shouldBeTheSame
--         "guards"
--         "let f = let foo x | x == 2 = 3 | else = 5 in let foo y = 4 in foo"
--     ]

-- -- TODO ∷ need strings
-- constantTests :: T.TestTree
-- constantTests =
--   T.testGroup
--     "constant tests"
--     [shouldBeTheSame "integers" "let f = 3"]

-- ------------------------------------------------------------
-- -- Test Helper functions
-- ------------------------------------------------------------

-- parseDesugarML :: ByteString -> [Target.TopLevel]
-- parseDesugarML =
--   MLPasses.op . ignoreHeader . Parser.parse

-- parseDesugarSexp :: ByteString -> [Sexp.T]
-- parseDesugarSexp = Desugar.op . parsedSexp

-- parsedSexp :: ByteString -> [Sexp.T]
-- parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

-- ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
-- ignoreHeader (Right (Frontend.NoHeader xs)) = xs
-- ignoreHeader _ = error "not no header"

-- ignoreRight :: Either a p -> p
-- ignoreRight (Right x) = x
-- ignoreRight (Left _) = error "not right"
