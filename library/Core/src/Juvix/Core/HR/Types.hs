{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.Core.HR.Types
  ( T,
    Term,
    pattern Star,
    pattern PrimTy,
    pattern Prim,
    pattern Pi,
    pattern Lam,
    pattern Sig,
    pattern Pair,
    pattern Let,
    pattern UnitTy,
    pattern Unit,
    pattern Elim,
    Elim,
    pattern Var,
    pattern App,
    pattern Ann,
    PPAnn' (..),
    PPAnn,
  )where

import Juvix.Library
import Juvix.Core.HR.Extend
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T

IR.extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Sig π x s t = Sig0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE
    Star, PrimTy, Prim, Pi, Lam, Sig, Pair, Let, UnitTy, Unit, Elim
  #-}

IR.extendElim "Elim" [] [t|T|] extElim


-- | Annotations for syntax highlighting
data PPAnn'
  = ATyCon   -- ^ builtin type constructors: Π, *, etc
  | AValCon  -- ^ builtin data: λ, (), usages, etc
  | APrimTy  -- ^ backend primitive type
  | APrimVal -- ^ backend primitive value
  | APunct   -- ^ brackets, dots, etc
  | AName    -- ^ names (TODO: distinguish bound & free?)

type PPAnn = Last PPAnn'

type instance PP.Ann (Term _ _) = PPAnn
type instance PP.Ann (Elim _ _) = PPAnn

type PrimPretty1 p = (PP.PrettySyntax p, PP.Ann p ~ PPAnn)
type PrimPretty ty val = (PrimPretty1 ty, PrimPretty1 val)

type Doc = PP.Doc PPAnn

annotate :: PPAnn' -> Doc -> Doc
annotate = PP.annotate . Last . Just

parens :: Doc -> Doc
parens = annotate APunct "(" `PP.enclose` annotate APunct ")"

colon :: Doc
colon = annotate APunct ":"

parensP :: PP.PrecReader m => PP.Prec -> m Doc -> m Doc
parensP p d = do
  p' <- ask @"prec"
  if p >= p' then d else parens <$> d

name :: NameSymbol.T -> Doc
name = annotate AName . PP.string . unintern . NameSymbol.toSymbol

liftPP :: PP.Doc () -> Doc
liftPP = fmap $ const $ Last Nothing

instance PrimPretty primTy primVal => PP.PrettySyntax (Term primTy primVal) where
  prettyPrec' = \case
    Star i -> parensP PP.FunArg $
      pure $ annotate ATyCon $ PP.sep ["*", PP.show i]
    PrimTy ty -> PP.prettyPrec' ty
    Prim val -> PP.prettyPrec' val
    Pi π x s t -> _
    Lam x t -> _
    Sig π x s t -> _
    Pair s t -> _
    Let π x b t -> _
    UnitTy -> _
    Unit -> _
    Elim e -> PP.prettyPrec' e


data Bind = PI | SIG
type Binder primTy primVal = (Bind, Usage.T, NameSymbol.T, Term primTy primVal)

getBinds ::
  PrimPretty primTy primVal =>
  Term primTy primVal -> ([Binder primTy primVal], Term primTy primVal)
getBinds = go [] where
  go acc (Pi π x s t) = go ((PI, π, x, s) : acc) t
  go acc (Sig π x s t) = go ((SIG, π, x, s) : acc) t
  go acc t = (reverse acc, t)

ppBinders ::
  (PrimPretty primTy primVal, PP.PrecReader m) =>
  [Binder primTy primVal] -> Term primTy primVal -> m Doc
ppBinders bs t = PP.hangA 2 (PP.sepA $ map ppBinder1 bs) (ppOuter t)
  where
    ppOuter = PP.withPrec PP.Outer . PP.prettyPrec'
    ppBinder1 (b, π, x, s) = PP.hcatA $
      [ ppBind b,
        parens <$> PP.hsepA [ppUsage π, pure $ name x, pure colon, ppOuter t]
      ]
    ppBind = _
    ppUsage = fmap (annotate AValCon . liftPP) . PP.prettyPrec'

instance
  ( PP.PrettySyntax primTy,
    PP.PrettySyntax primVal
  ) => PP.PrettySyntax (Elim primTy primVal) where
  prettyPrec' = _
