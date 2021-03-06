{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisations.Naturals where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>), natVal)
import qualified Juvix.Library.PrettyPrint as PP
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Show
import Prelude (String)

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val Natural -- c
  | Add -- f addition
  | Sub -- f subtraction
  | Mul -- f multiplication
  | Curried Val Natural
  deriving (Eq)

instance Show Val where
  show (Val x) = "Nat " <> Text.Show.show x
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show (Curried x y) = Juvix.Library.show x <> " " <> Text.Show.show y

typeOf :: Val -> P.PrimType Ty
typeOf (Val _) = Ty :| []
typeOf (Curried _ _) = Ty :| [Ty]
typeOf Add = Ty :| [Ty, Ty]
typeOf Sub = Ty :| [Ty, Ty]
typeOf Mul = Ty :| [Ty, Ty]

hasType :: Val -> P.PrimType Ty -> Bool
hasType x ty = ty == typeOf x

instance P.CanApply Ty where
  arity Ty = 0
  apply f xs = Left $ P.ExtraArguments f xs

instance P.CanApply Val where
  arity = pred . fromIntegral . length . typeOf
  apply f xs = app f $ toList xs
    where
      app Add (Val x : xs) = app (Curried Add x) xs
      app Sub (Val x : xs) = app (Curried Sub x) xs
      app Mul (Val x : xs) = app (Curried Mul x) xs
      app (Curried Add x) (Val y : ys) = app (Val (x + y)) ys
      app (Curried Sub x) (Val y : ys) = app (Val (x - y)) ys
      app (Curried Mul x) (Val y : ys) = app (Val (x * y)) ys
      app n [] = Right n
      app f (x : xs) = Left $ P.ExtraArguments f (x :| xs)

instance P.CanApply (Typed.TypedPrim Ty Val) where
  arity (App.Cont {numLeft}) = numLeft
  arity (App.Return {retTerm}) = P.arity retTerm

  -- partial application handled by Val so Cont should never appear
  apply (App.Return {retTerm = f}) xs'
    | Just xs <- traverse unReturn xs' =
      P.mapApplyErr wrap $ P.apply f xs
    where
      unReturn (App.Return {retTerm}) = Just retTerm
      unReturn _ = Nothing
      wrap x = App.Return {retTerm = x, retType = typeOf x}
  apply f' xs' = Left $ P.InvalidArguments f' xs'

instance E.HasWeak Ty where weakBy' _ _ ty = ty

instance Monoid (IR.XVPrimTy ext Ty val) => E.HasSubstValue ext Ty val Ty where
  substValueWith _ _ _ ty = pure $ IR.VPrimTy' ty mempty

instance Monoid (IR.XPrimTy ext Ty val) => E.HasPatSubstTerm ext Ty val Ty where
  patSubstTerm' _ _ ty = pure $ IR.PrimTy' ty mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (IR.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ IR.VPrim' val mempty

instance Monoid (IR.XPrim ext Ty Val) => E.HasPatSubstTerm ext Ty Val Val where
  patSubstTerm' _ _ val = pure $ IR.Prim' val mempty

parseTy :: Token.GenTokenParser String () Identity -> Parser Ty
parseTy lexer = do
  Token.reserved lexer "Nat"
  pure Ty

parseVal :: Token.GenTokenParser String () Identity -> Parser Val
parseVal lexer =
  parseNat lexer <|> parseAdd lexer <|> parseSub lexer <|> parseMul lexer

parseNat :: Token.GenTokenParser String () Identity -> Parser Val
parseNat lexer = Val . fromIntegral |<< Token.natural lexer

parseAdd :: Token.GenTokenParser String () Identity -> Parser Val
parseAdd lexer = Token.reserved lexer "add" >> pure Add

parseSub :: Token.GenTokenParser String () Identity -> Parser Val
parseSub lexer = Token.reserved lexer "sub" >> pure Sub

parseMul :: Token.GenTokenParser String () Identity -> Parser Val
parseMul lexer = Token.reserved lexer "mul" >> pure Mul

reservedNames :: [String]
reservedNames = ["Nat", "add", "sub", "mul"]

reservedOpNames :: [String]
reservedOpNames = []

isNat :: Integer -> Bool
isNat i = i >= 0

natVal :: Integer -> Maybe Val
natVal i = if i >= 0 then Just (Val (fromIntegral i)) else Nothing

builtinTypes :: P.Builtins Ty
builtinTypes = HM.fromList [("Nat" :| [], Ty)]

builtinValues :: P.Builtins Val
builtinValues =
  HM.fromList [
    ("add" :| [], Add),
    ("sub" :| [], Sub),
    ("mul" :| [], Mul)
  ]

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      parseTy,
      parseVal,
      reservedNames,
      reservedOpNames,
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \i _ -> isNat i,
      intVal = natVal,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }

type instance PP.Ann Ty = ()

instance PP.PrettySyntax Ty where prettyPrec' Ty = pure "Nat"


data PPAnn' = Lit | Fun | Paren deriving (Eq, Ord, Show)
type PPAnn = Last PPAnn'

type Doc = PP.Doc PPAnn

type instance PP.Ann Val = PPAnn

nat :: Natural -> Doc
nat = PP.annotate' Lit . PP.show

instance PP.PrettySyntax Val where
  prettyPrec' = \case
    Val k -> pure $ nat k
    Add -> pure $ PP.annotate' Fun "add"
    Sub -> pure $ PP.annotate' Fun "sub"
    Mul -> pure $ PP.annotate' Fun "mul"
    Curried f k -> PP.parensP' Paren PP.FunArg $
      PP.hsepA [
        PP.withPrec PP.FunArg $ PP.prettyPrec' f,
        pure $ nat k
      ]
