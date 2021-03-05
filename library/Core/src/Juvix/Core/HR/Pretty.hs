{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Core.HR.Pretty
  ( PPAnn' (..),
    PPAnn,
  ) where

import Juvix.Library
import Juvix.Core.HR.Types
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage


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

angles :: Doc -> Doc
angles = annotate APunct "‹" `PP.enclose` annotate APunct "›"

comma :: Doc
comma = annotate APunct ","

colon :: Doc
colon = annotate APunct ":"

arrow :: Doc
arrow = annotate APunct "→"

equals :: Doc
equals = annotate APunct "="

pipe :: Doc
pipe = annotate APunct "|"

parensP :: PP.PrecReader m => PP.Prec -> m Doc -> m Doc
parensP p d = do
  p' <- ask @"prec"
  if p >= p' then d else parens <$> d

name :: NameSymbol.T -> Doc
name = annotate AName . PP.string . unintern . NameSymbol.toSymbol

liftPP :: PP.Doc () -> Doc
liftPP = fmap $ const $ Last Nothing

data Bind = PI | SIG
type Binder primTy primVal = (Bind, Usage.T, NameSymbol.T, Term primTy primVal)

getBinds ::
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
    ppBinder1 (b, π, x, s) = PP.hcatA $
      [ ppBind b,
        parens <$> PP.hsepA [ppUsage π, pure $ name x, pure colon, ppOuter s],
        pure arrow
      ]
    ppBind = pure . annotate ATyCon . \case PI -> "Π"; SIG -> "Σ"

ppUsage :: PP.PrecReader m => Usage.T -> m Doc
ppUsage = fmap (annotate AValCon . liftPP) . PP.prettyPrec'

getLams :: Term primTy primVal -> ([NameSymbol.T], Term primTy primVal)
getLams = go [] where
  go acc (Lam x t) = go (x : acc) t
  go acc t = (reverse acc, t)

ppLams ::
  (PrimPretty primTy primVal, PP.PrecReader m) =>
  [NameSymbol.T] -> Term primTy primVal -> m Doc
ppLams bs t = PP.hangA 2 (PP.sepA $ map ppLam1 bs) (ppOuter t)
  where
    ppLam1 x = pure $ PP.sep [annotate AValCon "λ", name x, arrow]

ppOuter :: (PP.PrecReader m, PP.PrettySyntax a) => a -> m (PP.Doc (PP.Ann a))
ppOuter = PP.withPrec PP.Outer . PP.prettyPrec'


getApps :: Elim primTy primVal -> (Elim primTy primVal, [Term primTy primVal])
getApps = go [] where
  go acc (App f s) = go (s : acc) f
  go acc e = (e, reverse acc)


instance PrimPretty primTy primVal => PP.PrettySyntax (Term primTy primVal) where
  prettyPrec' = \case
    Star i -> parensP PP.FunArg $
      pure $ annotate ATyCon $ PP.sep ["*", PP.show i]
    PrimTy ty -> PP.prettyPrec' ty
    Prim val -> PP.prettyPrec' val
    Pi π x s t -> ppBinders ((PI, π, x, s) : bs) body
      where (bs, body) = getBinds t
    Lam x t -> ppLams (x : xs) body
      where (xs, body) = getLams t
    Sig π x s t -> ppBinders ((SIG, π, x, s) : bs) body
      where (bs, body) = getBinds t
    Pair s t ->
      fmap angles $ PP.sepA [
        PP.hcatA [ppOuter s, pure comma],
        ppOuter t
      ]
    Let π x b t ->
      PP.sepA [
        PP.sepA [
          pure $ annotate AValCon "let",
          PP.hangA 2
            (PP.sepA [ppUsage π, pure $ name x, pure equals])
            (ppOuter b),
          pure $ annotate AValCon "in"
        ],
        ppOuter t
      ]
    UnitTy -> pure $ annotate ATyCon "Unit"
    Unit -> pure $ annotate AValCon "⌷"
    Elim e -> PP.prettyPrec' e

instance PrimPretty primTy primVal => PP.PrettySyntax (Elim primTy primVal) where
  prettyPrec' = \case
    Var x -> pure $ annotate AName $ name x
    App f s ->
      Elim f' : ss ++ [s]
      |> map (PP.withPrec PP.FunArg . PP.prettyPrec')
      |> PP.sepA
      where (f', ss) = getApps f
    Ann π s a ℓ -> fmap parens $
      PP.hangsA 2
        (PP.hsepA [ppUsage π, pure pipe, ppOuter s])
        [ PP.hsepA [pure colon, ppOuter a],
          PP.hsepA [pure colon, ppOuter (Star ℓ :: Term primTy primVal)]
        ]
