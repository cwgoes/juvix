{-# LANGUAGE ApplicativeDo #-}

-- |
-- - The front end parser for the Juvix Programming language
--
-- - Parsers with S at the end, eat the spaces at the end of the parse
--
-- - Parsers with SN at the end, eats the spaces and new lines at the
--   end of the parse
module Juvix.Frontend.Parser where

-- ( parse,
--   expressionSN,
--   removeComments,
--   topLevelSN,
--   expression,
--   matchLogic,
--   cond,
--   prefixSymbol,
-- )

import Control.Arrow (left)
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Encoding
import Data.Word8 (isDigit)
import qualified Juvix.Frontend.Types as Types
import qualified Juvix.Frontend.Types.Base as Types
import Juvix.Library hiding (guard, list, mod, product, sum)
import Juvix.Library.Parser (Parser, ParserError, skipLiner, spaceLiner, spacer)
import qualified Juvix.Library.Parser as J
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import Prelude (fail)

--------------------------------------------------------------------------------
-- Top Level Runner
--------------------------------------------------------------------------------

prettyParse :: ByteString -> Either [Char] (Types.Header Types.TopLevel)
prettyParse = left P.errorBundlePretty . parse

parse :: ByteString -> Either ParserError (Types.Header Types.TopLevel)
parse = P.parse (J.eatSpaces (header <* P.eof)) "" . removeComments

--------------------------------------------------------------------------------
-- Pre-Process
--------------------------------------------------------------------------------

removeComments :: ByteString -> ByteString
removeComments = ByteString.concat . grabComments
  where
    onBreakDo _break _con "" = []
    onBreakDo break cont str = break str |> cont
    -- TODO ∷ Make faster
    grabComments = breakComment `onBreakDo` f
      where
        f (notIn, in') =
          notIn : grabComments (dropNewLine in')
    dropNewLine =
      ByteString.dropWhile (not . (== J.newLine))

-- These two functions have size 4 * 8 = 32 < Bits.finiteBitSize (0 :: Word) = 64
-- thus this compiles to a shift
breakComment :: ByteString -> (ByteString, ByteString)
breakComment = ByteString.breakSubstring "--"

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

header :: Parser (Types.Header Types.TopLevel)
header = P.try headerCase <|> noHeaderCase

headerCase :: Parser (Types.Header Types.TopLevel)
headerCase = do
  reserved "mod"
  name <- prefixSymbolDotSN
  reserved "where"
  Types.Header name <$> P.some topLevelSN

noHeaderCase :: Parser (Types.Header Types.TopLevel)
noHeaderCase = do
  Types.NoHeader <$> P.some topLevelSN

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

topLevel :: Parser Types.TopLevel
topLevel =
  P.try (Types.Type <$> typeP)
    <|> P.try fun
    <|> P.try modT
    <|> P.try (Types.ModuleOpen <$> moduleOpen)
    <|> P.try (Types.Signature <$> signature')
    <|> P.try (Types.Declaration <$> declaration)

expressionGen' ::
  Parser Types.Expression -> Parser Types.Expression
expressionGen' p =
  P.try (Types.Cond <$> cond)
    <|> P.try (Types.Let <$> let')
    <|> P.try (Types.LetType <$> letType)
    <|> P.try (Types.ModuleE <$> mod)
    <|> P.try (Types.Match <$> match)
    <|> P.try (Types.OpenExpr <$> moduleOpenExpr)
    <|> P.try (Types.Block <$> block)
    <|> P.try (Types.Lambda <$> lam)
    <|> P.try (Types.Primitive <$> primitives)
    <|> P.try (Types.DeclarationE <$> declarationExpression)
    <|> P.try p
    <|> expressionArguments

expressionArguments :: Parser Types.Expression
expressionArguments =
  P.try (Types.Block <$> block)
    <|> P.try (Types.ExpRecord <$> expRecord)
    <|> P.try (Types.Constant <$> constant)
    -- <|> try (Types.NamedTypeE <$> namedRefine)
    <|> P.try (Types.Name <$> prefixSymbolDot)
    <|> P.try universeSymbol
    <|> P.try (Types.List <$> list)
    -- We wrap this in a paren to avoid conflict
    -- with infixity that we don't know about at this phase!
    <|> tupleParen

do''' :: Parser Types.Expression
do''' = Types.Do <$> do'

app'' :: Parser Types.Expression
app'' = Types.Application <$> P.try application

all'' :: Parser Types.Expression
all'' = P.try do''' <|> app''

expressionGen :: Parser Types.Expression -> Parser Types.Expression
expressionGen p =
  Expr.makeExprParser (spaceLiner (expressionGen' p)) tableExp

-- used to remove do from parsing
expression' :: Parser Types.Expression
expression' = expressionGen app''

-- used to remove app from parsing
expression'' :: Parser Types.Expression
expression'' = expressionGen do'''

-- used to remove both from parsing
expression''' :: Parser Types.Expression
expression''' = expressionGen (fail "")

expression :: Parser Types.Expression
expression = expressionGen all''

usage :: Parser Types.Expression
usage = P.string "u#" *> expression

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

declaration :: Parser Types.Declaration
declaration = do
  reserved "declare"
  Types.Infixivity <$> infixDeclar

declarationExpression :: Parser Types.DeclarationExpression
declarationExpression =
  Types.DeclareExpression <$> declaration <*> (reserved "in" *> expression)

infixDeclar :: Parser Types.InfixDeclar
infixDeclar = do
  _ <- P.string "infix"
  f <-
    P.try (Types.AssocL <$ P.string "l")
      <|> P.try (Types.AssocR <$ P.string "r")
      <|> pure Types.NonAssoc
  symbolEnd
  name <- prefixSymbolSN
  f name . fromInteger <$> spaceLiner J.integer

--------------------------------------------------------------------------------
-- Modules/ Function Gen
--------------------------------------------------------------------------------

functionModStartReserved ::
  ByteString -> (Symbol -> [Types.Arg] -> Parser a) -> Parser a
functionModStartReserved str f = do
  reserved str
  name <- prefixSymbolSN
  args <- P.many argSN
  f name args

functionModGen :: Parser a -> Parser (Types.FunctionLike a)
functionModGen p =
  functionModStartReserved
    "let"
    ( \name args -> do
        guard <- guard p
        pure (Types.Like name args guard)
    )

--------------------------------------------------
-- Guard
--------------------------------------------------

guard :: Parser a -> Parser (Types.GuardBody a)
guard p =
  P.try (Types.Guard <$> condB p)
    <|> P.try (Types.Body <$> (J.skipLiner J.equals *> p))

--------------------------------------------------
-- Args
--------------------------------------------------

arg :: Parser Types.Arg
arg =
  P.try (Types.ImplicitA <$> (P.char J.hash *> matchLogic))
    <|> (Types.ConcreteA <$> matchLogic)

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

signature' :: Parser Types.Signature
signature' = do
  reserved "sig"
  name <- prefixSymbolSN
  maybeUsage <-
    P.optional (fmap Types.Constant constantSN <|> spaceLiner (J.parens expressionSN))
  skipLiner J.colon
  typeclasses <- signatureConstraintSN
  exp <- expression
  pure (Types.Sig name maybeUsage exp typeclasses)

signatureConstraint :: Parser [Types.Expression]
signatureConstraint =
  P.try (pure <$> expression <* reserved "=>")
    <|> P.try (J.parens (P.sepBy expression (skipLiner J.comma)) <* reserved "=>")
    <|> pure []

--------------------------------------------------------------------------------
-- Match
--------------------------------------------------------------------------------

match :: Parser Types.Match
match = do
  reserved "case"
  matchOn <- expressionSN
  reserved "of"
  matchs <- J.many1H (P.try matchLSN)
  pure (Types.Match'' matchOn matchs)

matchL :: Parser Types.MatchL
matchL = do
  skipLiner J.pipe
  match <- P.try matchLogicSN
  spaceLiner (P.string "->")
  Types.MatchL match <$> P.try expression

matchLogic :: Parser Types.MatchLogic
matchLogic = J.maybeParend (P.try matchLogicNamedSN <|> matchLogicNotNamedSN)

matchLogicNamed :: Parser Types.MatchLogic
matchLogicNamed = do
  name <- prefixSymbol
  skipLiner J.at
  start <- J.maybeParend matchLogicStartSN
  pure (Types.MatchLogic start (Just name))

matchLogicNotNamed :: Parser Types.MatchLogic
matchLogicNotNamed = do
  start <- matchLogicStart
  pure (Types.MatchLogic start Nothing)

matchLogicStart :: Parser Types.MatchLogicStart
matchLogicStart =
  P.try matchRecord
    <|> P.try matchCon
    <|> P.try matchName
    <|> matchConstant

matchConstant :: Parser Types.MatchLogicStart
matchConstant = Types.MatchConst <$> constant

matchCon :: Parser Types.MatchLogicStart
matchCon = do
  con <- prefixCapitalDotSN
  matchd <- P.many matchLogicSN
  pure (Types.MatchCon con matchd)

matchName :: Parser Types.MatchLogicStart
matchName = Types.MatchName <$> prefixSymbol

matchRecord :: Parser Types.MatchLogicStart
matchRecord =
  Types.MatchRecord <$> nameSetMany matchLogic

--------------------------------------------------------------------------------
-- NameSet
--------------------------------------------------------------------------------
nameSetMany' :: Parser a -> Parser (NonEmpty (Types.NameSet a))
nameSetMany' parser =
  J.curly $ do
    x <- J.sepBy1H (nameSetSN parser) (skipLiner J.comma)
    if  | length x == 1 && isPunned x ->
          x <$ skipLiner J.comma
        | otherwise ->
          x <$ P.optional (skipLiner J.comma)

isPunned :: NonEmpty (Types.NameSet t) -> Bool
isPunned (Types.Punned {} :| _) = True
isPunned (Types.NonPunned {} :| _) = False

nameSetMany :: Parser a -> Parser (NonEmpty (Types.NameSet a))
nameSetMany parser =
  J.curly (J.sepBy1HFinal (nameSetSN parser) (skipLiner J.comma))

nameSet :: Parser a -> Parser (Types.NameSet a)
nameSet parser = P.try (nameMatch parser) <|> namePunned

namePunned :: Parser (Types.NameSet a)
namePunned = Types.Punned <$> prefixSymbolDot

nameMatch :: Parser a -> Parser (Types.NameSet a)
nameMatch parser = do
  name <- prefixSymbolDotSN
  J.skipLiner J.equals
  bound <- parser
  pure (Types.NonPunned name bound)

--------------------------------------------------------------------------------
-- Modules and Functions
--------------------------------------------------------------------------------

genGuard :: Symbol -> [Types.Arg] -> Parser a -> Parser (Types.FunctionLike a)
genGuard n a p =
  guard p >>| Types.Like n a

fun :: Parser Types.TopLevel
fun = functionModStartReserved "let" func
  where
    func n a =
      genGuard n a expression
        >>| Types.Function . Types.Func

modT :: Parser Types.TopLevel
modT = functionModStartReserved "mod" mod
  where
    mod n a =
      ( genGuard n a (J.many1H topLevelSN)
          >>| Types.Module . Types.Mod
      )
        <* reserved "end"

moduleOpen :: Parser Types.ModuleOpen
moduleOpen = do
  reserved "open"
  Types.Open <$> moduleName

moduleName :: Parser Types.ModuleName
moduleName =
  prefixSymbolDot

moduleOpenExpr :: Parser Types.ModuleOpenExpr
moduleOpenExpr = P.try moduleOpenExprNormal <|> moduleOpenExprParens

moduleOpenExprParens :: Parser Types.ModuleOpenExpr
moduleOpenExprParens = do
  -- we want it to fail at the ., since it's a paren after it
  -- Int.()
  name <- prefixSymbolDotPermissive
  P.char J.dot
  expr <- J.parens expression
  pure (Types.OpenExpress name expr)

moduleOpenExprNormal :: Parser Types.ModuleOpenExpr
moduleOpenExprNormal = do
  reserved "open"
  name <- moduleNameSN
  reserved "in"
  Types.OpenExpress name <$> expression

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

typeP :: Parser Types.Type
typeP = do
  reserved "type"
  usag <- P.optional usage
  name <- prefixSymbolSN
  args <- P.many prefixSymbolSN
  Types.Typ usag name args <$> dataParser

dataParser :: Parser Types.Data
dataParser = do
  arrow <- P.optional (skipLiner J.colon *> expression)
  J.skipLiner J.equals
  adt <- adt
  case arrow of
    Just arr -> pure (Types.Arrowed arr adt)
    Nothing -> pure (Types.NonArrowed adt)

--------------------------------------------------
-- ADT parser
--------------------------------------------------

adt :: Parser Types.Adt
adt =
  P.try (Types.Sum <$> (P.optional (skipLiner J.pipe) *> J.sepBy1H sumSN (skipLiner J.pipe)))
    <|> (Types.Product <$> standAloneProduct)

sum :: Parser Types.Sum
sum = Types.S <$> prefixSymbolSN <*> P.optional product

standAloneProduct :: Parser Types.Product
standAloneProduct =
  Types.Record <$> record

product :: Parser Types.Product
product =
  P.try (Types.Record <$> record)
    <|> P.try (skipLiner J.colon *> fmap Types.Arrow expression)
    <|> fmap Types.ADTLike (P.many (P.try expression'''SN))

record :: Parser Types.Record
record = do
  names <-
    spaceLiner
      $ J.curly
      $ J.sepBy1HFinal nameTypeSN (skipLiner J.comma)
  familySignature <- P.optional (skipLiner J.colon *> expression)
  pure (Types.Record'' names familySignature)

nameType :: Parser Types.NameType
nameType = do
  name <- nameParserSN
  skipLiner J.colon
  sig <- expression
  pure (Types.NameType' sig name)

-- nameParserColon :: Parser Types.Name
-- nameParserColon =
--   nameParserSN <* skip (== J.colon)

nameParser :: Parser Types.Name
nameParser =
  P.try (P.skipSome (P.char J.hash) *> fmap Types.Implicit prefixSymbol)
    <|> Types.Concrete <$> prefixSymbol

--------------------------------------------------
-- Arrow Type parser
--------------------------------------------------

-- namedRefine :: Parser Types.NamedType
-- namedRefine =
--   Types.NamedType <$> nameParserColonSN <*> expression

--------------------------------------------------
-- TypeNameParser and typeRefine Parser
--------------------------------------------------

universeSymbol :: Parser Types.Expression
universeSymbol = do
  _ <- P.string "u#"
  Types.UniverseName <$> universeExpression

universeExpression :: Parser Types.UniverseExpression
universeExpression =
  P.try (Types.UniverseExpression <$> prefixSymbolSN)
    -- TODO ∷ make this proper do + and max!
    <|> Types.UniverseExpression <$> J.parens prefixSymbolSN

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

block :: Parser Types.Block
block = do
  reserved "begin"
  exp <- expressionSN
  reserved "end"
  pure (Types.Bloc exp)

--------------------------------------------------
-- Records
--------------------------------------------------

expRecord :: Parser Types.ExpRecord
expRecord = Types.ExpressionRecord <$> nameSetMany' expression

--------------------------------------------------
-- Let
--------------------------------------------------

mod :: Parser Types.ModuleE
mod = do
  reserved "mod"
  name <- prefixSymbolSN
  args <- P.many argSN
  guarded <- guard (J.many1H topLevelSN)
  reserved "end"
  reserved "in"
  Types.ModE (Types.Like name args guarded) <$> expression

let' :: Parser Types.Let
let' = do
  binds <- functionModGen expression
  reserved "in"
  Types.Let'' binds <$> expression

letType :: Parser Types.LetType
letType = do
  reserved "let"
  typ <- typePSN
  reserved "in"
  Types.LetType'' typ <$> expression

--------------------------------------------------
-- Cond
--------------------------------------------------

cond :: Parser (Types.Cond Types.Expression)
cond = do
  reserved "if"
  condB expression

condB :: Parser a -> Parser (Types.Cond a)
condB p = Types.C <$> J.many1H (P.try $ condLogicSN p)

condLogic :: Parser a -> Parser (Types.CondLogic a)
condLogic p = do
  J.skipLiner J.pipe
  pred <- expressionSN
  J.skipLiner J.equals
  Types.CondExpression pred <$> p

--------------------------------------------------
-- Lambda
--------------------------------------------------

lam :: Parser Types.Lambda
lam = do
  skipLiner J.backSlash
  args <- J.many1H matchLogicSN
  reserved "->"
  body <- expression
  pure (Types.Lamb args body)

--------------------------------------------------
-- Application
--------------------------------------------------

application :: Parser Types.Application
application = do
  name <- spaceLiner (expressionGen' (fail ""))
  args <- J.many1H (spaceLiner expressionArguments)
  pure (Types.App name args)

--------------------------------------------------
-- Literals
--------------------------------------------------

primitives :: Parser Types.Primitive
primitives = do
  _ <- P.single J.percent
  Types.Prim <$> prefixSymbolDot

list :: Parser Types.List
list = Types.ListLit <$> J.brackets (P.sepBy expression (skipLiner J.comma))

tupleParen :: Parser Types.Expression
tupleParen = do
  p <- J.parens (P.sepBy1 (expressionGen all'') (skipLiner J.comma))
  case p of
    [] -> fail "doesn't happen"
    [x] -> pure (Types.Parened x)
    _ : _ -> pure (Types.Tuple (Types.TupleLit p))

constant :: Parser Types.Constant
constant =
  (Types.Number <$> number)
    <|> Types.String <$> string'

number :: Parser Types.Numb
number =
  P.try (Types.Integer' <$> J.integer)
    <|> Types.Double' <$> float

digits :: Parser ByteString
digits = P.takeWhile1P (Just "digits") isDigit

float :: Parser Double
float = do
  _s1 <- digits
  void (P.char J.dot)
  _s2 <- digits
  fail "float not implemented"

--   pure (read (s1 <> "." <> s2))

-- TODO ∷ no escape for strings yet
string' :: Parser Types.String'
string' = do
  P.single J.quote
  words <- P.takeWhile1P (Just "Not quote") (/= J.quote)
  P.single J.quote
  pure (Types.Sho $ Encoding.decodeUtf8 words)

--------------------------------------------------
-- Do
--------------------------------------------------

do' :: Parser Types.Do
do' = do
  doExp <- do''
  case length doExp of
    1 -> fail "do expression with only 1 value"
    0 -> fail "parser faled with empty list"
    _ -> pure (Types.Do'' $ NonEmpty.fromList doExp)

doBind :: Parser [Types.DoBody]
doBind = do
  name <- prefixSymbolSN
  spaceLiner (P.string "<-")
  body <- expression'SN
  pure [Types.DoBody (Just name) body]

doNotBind :: Parser [Types.DoBody]
doNotBind = do
  body <- expression'SN
  pure [Types.DoBody Nothing body]

do'' :: Parser [Types.DoBody]
do'' = Expr.makeExprParser (P.try doBind <|> doNotBind) table P.<?> "bind expr"

--------------------------------------------------------------------------------
-- Symbol Handlers
--------------------------------------------------------------------------------

infixSymbolGen :: Parser Symbol -> Parser Symbol
infixSymbolGen p = do
  symb <- p
  if  | Set.member symb J.reservedSymbols -> fail "symbol is reserved word"
      | otherwise -> pure symb

infixSymbolDot :: Parser (NonEmpty Symbol)
infixSymbolDot = do
  qualified <- P.option [] (NonEmpty.toList <$> prefixSymbolDotPermissive <* P.char J.dot)
  -- -o is a bit special since it's a normal letter
  -- this is a bit of a hack
  infix' <- P.try ("-o" <$ P.string "-o") <|> P.try infixSymbol
  pure (NonEmpty.fromList (qualified <> [infix']))

infixSymbol :: Parser Symbol
infixSymbol = infixSymbolGen (P.try infixSymbol' <|> infixPrefix)

infixSymbol' :: Parser Symbol
infixSymbol' =
  internText . Encoding.decodeUtf8 <$> P.takeWhile1P (Just "Valid Infix Symbol") J.validInfixSymbol

infixPrefix :: Parser Symbol
infixPrefix =
  P.single J.backtick *> prefixSymbol <* P.single J.backtick

prefixSymbolGen :: Parser Word8 -> Parser Symbol
prefixSymbolGen startParser = do
  start <- startParser
  rest <- P.takeWhileP (Just "Valid Middle Symbol") J.validMiddleSymbol
  -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
  let new = ByteString.cons start rest
  if  | Set.member new J.reservedWords -> fail "symbol is reserved operator"
      | otherwise -> pure (internText $ Encoding.decodeUtf8 new)

symbolEndGen :: ByteString -> Parser ()
symbolEndGen _s = do
  P.notFollowedBy (P.satisfy J.validMiddleSymbol)
  P.takeWhileP (Just "Empty Check") J.emptyCheck *> pure ()

symbolEnd :: Parser ()
symbolEnd = symbolEndGen "current symbol is not over"

reserved :: ByteString -> Parser ()
reserved res =
  P.string res *> symbolEndGen "symbol is not the reserved symbol"

-- TODO ∷ this may be bad
-- this allows "(*).Foo.(<*>)" to be accepted
-- Though Should we allow this since, these are prefix if spelled this way
-- we don't enforce capitalization, and thus it would be improper for to
-- special case it out!

prefixSepGen :: Parser Symbol -> Parser (NonEmpty Symbol)
prefixSepGen parser = do
  ret <- J.sepBy1H parser (P.char J.dot)
  v <- P.optional $ P.try P.eof
  case v of
    Nothing -> do
      d <- P.optional $ P.lookAhead (P.char J.dot)
      case d of
        Nothing -> pure ret
        Just _ -> fail "symbol not prefix"
    Just _ -> pure ret

-- the permissive functions allow the functions to not fully parse the word
-- useful for infix application
prefixSymbolDotPermissive :: Parser (NonEmpty Symbol)
prefixSymbolDotPermissive = J.sepBy1H prefixSymbol (P.char J.dot)

prefixCapitalDotPermissive :: Parser (NonEmpty Symbol)
prefixCapitalDotPermissive = J.sepBy1H prefixCapital (P.char J.dot)

prefixSymbolDot :: Parser (NonEmpty Symbol)
prefixSymbolDot = prefixSepGen prefixSymbol

prefixCapitalDot :: Parser (NonEmpty Symbol)
prefixCapitalDot = prefixSepGen prefixCapital

prefixSymbol :: Parser Symbol
prefixSymbol =
  P.try (prefixSymbolGen (P.satisfy J.validStartSymbol))
    <|> parend

parend :: Parser Symbol
parend =
  skipLiner J.openParen
    *> spaceLiner (infixSymbolGen infixSymbol') <* P.char J.closeParen

prefixCapital :: Parser Symbol
prefixCapital = prefixSymbolGen (P.satisfy J.validUpperSymbol)

--------------------------------------------------------------------------------
-- Expr Parser
--------------------------------------------------------------------------------

-- For Expressions
tableExp :: [[Expr.Operator Parser Types.Expression]]
tableExp =
  [ [refine],
    [infixOp],
    [arrowExp]
  ]

infixOp :: Expr.Operator Parser Types.Expression
infixOp =
  Expr.InfixR
    ( P.try $ do
        inf <- spaceLiner infixSymbolDot
        pure
          (\l r -> Types.Infix (Types.Inf l inf r))
    )

arrowExp :: Expr.Operator Parser Types.Expression
arrowExp =
  Expr.InfixR
    ( P.try $ do
        skipLiner J.dash
        exp <- expressionSN
        reserved "->"
        pure
          (\l r -> Types.ArrowE (Types.Arr' l exp r))
    )

refine :: Expr.Operator Parser Types.Expression
refine =
  Expr.Postfix
    $ P.try
    $ do
      refine <- spaceLiner (J.curly expressionSN)
      pure (\p -> Types.RefinedE (Types.TypeRefine p refine))

-- For Do!
table :: Semigroup a => [[Expr.Operator Parser a]]
table = [[binary ";" (<>)]]

binary :: ByteString -> (a -> a -> a) -> Expr.Operator Parser a
binary name fun = Expr.InfixL (fun <$ spaceLiner (P.string name))

--------------------------------------------------------------------------------
-- SN Derivatives
--------------------------------------------------------------------------------

topLevelSN :: Parser Types.TopLevel
topLevelSN = spaceLiner topLevel

expression'SN :: Parser Types.Expression
expression'SN = spaceLiner expression'

expression''SN :: Parser Types.Expression
expression''SN = spaceLiner expression''

expression'''SN :: Parser Types.Expression
expression'''SN = spaceLiner expression'''

-- TODO: Add Docs
expressionSN :: Parser Types.Expression
expressionSN = spaceLiner expression

expressionS :: Parser Types.Expression
expressionS = spacer expression

signatureConstraintSN :: Parser [Types.Expression]
signatureConstraintSN = spaceLiner signatureConstraint

argSN :: Parser Types.Arg
argSN = spaceLiner arg

matchLSN :: Parser Types.MatchL
matchLSN = spaceLiner matchL

matchLogicSN :: Parser Types.MatchLogic
matchLogicSN = spaceLiner matchLogic

matchLogicStartSN :: Parser Types.MatchLogicStart
matchLogicStartSN = spaceLiner matchLogicStart

matchLogicNamedSN :: Parser Types.MatchLogic
matchLogicNamedSN = spaceLiner matchLogicNamed

matchLogicNotNamedSN :: Parser Types.MatchLogic
matchLogicNotNamedSN = spaceLiner matchLogicNotNamed

nameSetSN :: Parser a -> Parser (Types.NameSet a)
nameSetSN = spaceLiner . nameSet

moduleNameSN :: Parser Types.ModuleName
moduleNameSN = spaceLiner moduleName

condLogicSN :: Parser a -> Parser (Types.CondLogic a)
condLogicSN = spaceLiner . condLogic

typePSN :: Parser Types.Type
typePSN = spaceLiner typeP

typePS :: Parser Types.Type
typePS = spacer typeP

recordSN :: Parser Types.Record
recordSN = spaceLiner record

recordS :: Parser Types.Record
recordS = spacer record

nameTypeSN :: Parser Types.NameType
nameTypeSN = spaceLiner nameType

-- nameParserColonSN :: Parser Types.Name
-- nameParserColonSN = spaceLiner nameParserColon

nameParserSN :: Parser Types.Name
nameParserSN = spaceLiner nameParser

-- namedRefineSN :: Parser Types.NamedType
-- namedRefineSN = spaceLiner namedRefine

sumSN :: Parser Types.Sum
sumSN = spaceLiner sum

sumS :: Parser Types.Sum
sumS = spaceLiner sum

prefixCapitalDotSN :: Parser (NonEmpty Symbol)
prefixCapitalDotSN = spaceLiner prefixCapitalDot

prefixCapitalSN :: Parser Symbol
prefixCapitalSN = spaceLiner prefixCapital

prefixSymbolDotSN :: Parser (NonEmpty Symbol)
prefixSymbolDotSN = spaceLiner prefixSymbolDot

prefixSymbolSN :: Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

prefixSymbolS :: Parser Symbol
prefixSymbolS = spacer prefixSymbol

constantSN :: Parser Types.Constant
constantSN = spaceLiner constant
