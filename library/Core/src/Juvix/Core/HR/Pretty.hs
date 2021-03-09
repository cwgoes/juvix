{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.HR.Pretty
  ( PPAnn' (..),
    PPAnn,
    ToPPAnn (..),
    Doc,
    PrimPretty1,
    PrimPretty,
  ) where

import Juvix.Library
import Juvix.Core.HR.Types
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Core.Parameterisations.Naturals as Nat


-- | Annotations for syntax highlighting
data PPAnn'
  = ATyCon   -- ^ builtin type constructors: Π, *, etc
  | AValCon  -- ^ builtin data: λ, (), usages, etc
  | APrimTy  -- ^ backend primitive type
  | APrimVal -- ^ backend primitive value
  | APrimFun -- ^ backend primitive function
  | APunct   -- ^ brackets, dots, etc
  | AName    -- ^ names (TODO: distinguish bound & free?)

type PPAnn = Last PPAnn'

type instance PP.Ann (Term _ _) = PPAnn
type instance PP.Ann (Elim _ _) = PPAnn

type PrimPretty1 p = (PP.PrettySyntax p, ToPPAnn (PP.Ann p))
type PrimPretty ty val = (PrimPretty1 ty, PrimPretty1 val)

type Doc = PP.Doc PPAnn

annotate :: PPAnn' -> Doc -> Doc
annotate = PP.annotate'

parens :: Doc -> Doc
parens = PP.parensA' APunct

parensP :: PP.PrecReader m => PP.Prec -> m Doc -> m Doc
parensP = PP.parensP' APunct

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
    ppBinder1 (b, π, x, s) = PP.hsepA $
      [ ppBind b,
        parens <$> PP.sepA [
          PP.hsepA [ppUsage π, pure pipe],
          pure $ PP.hsep [name x, colon],
          ppOuter s
        ],
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
ppLams names body =
    PP.hangA 2 (pure header) (ppOuter body)
  where
    header = PP.sep [annotate AValCon "λ", ppNames names, arrow]
    ppNames = PP.hsep . map (liftPP . PP.prettyText)

ppOuter :: (PP.PrecReader m, PP.PrettySyntax a) => a -> m (PP.Doc (PP.Ann a))
ppOuter = PP.withPrec PP.Outer . PP.prettyPrec'


getApps :: Elim primTy primVal -> (Elim primTy primVal, [Term primTy primVal])
getApps = go [] where
  go acc (App f s) = go (s : acc) f
  go acc e = (e, reverse acc)

ppPairs ::
  (PrimPretty primTy primVal, PP.PrecReader m) =>
  [Term primTy primVal] -> m Doc
ppPairs =
  fmap (angles . PP.sep . PP.punctuate comma) . traverse ppOuter

getPairs :: Term primTy primVal -> [Term primTy primVal]
getPairs (Pair s t) = s : getPairs t
getPairs t = [t]


instance PrimPretty primTy primVal => PP.PrettySyntax (Term primTy primVal) where
  prettyPrec' = \case
    Star i -> parensP PP.FunArg $
      pure $ annotate ATyCon $ PP.sep ["*", PP.show i]
    PrimTy ty ->
      annotate APrimTy . fmap toPPAnn <$> PP.prettyPrec' ty
    Prim val ->
      annotate APrimVal . fmap toPPAnn <$> PP.prettyPrec' val
    Pi π x s t -> ppBinders ((PI, π, x, s) : bs) body
      where (bs, body) = getBinds t
    Lam x t -> ppLams (x : xs) body
      where (xs, body) = getLams t
    Sig π x s t -> ppBinders ((SIG, π, x, s) : bs) body
      where (bs, body) = getBinds t
    Pair s t ->
      ppPairs (s : ts)
      where ts = getPairs t
    Let π x b t ->
      PP.sepA [
        PP.hsepA [
          PP.hangA 2
            (PP.hsepA [let_, ppUsage π, pure pipe, pure $ name x, pure equals])
            (ppOuter b),
          in_
        ],
        ppOuter t
      ]
      where
        let_ = pure $ annotate AValCon "let"
        in_ = pure $ annotate AValCon "in"
    UnitTy -> pure $ annotate ATyCon "Unit"
    Unit -> pure $ annotate AValCon "⌷"
    Elim e -> PP.prettyPrec' e

instance PrimPretty primTy primVal => PP.PrettySyntax (Elim primTy primVal) where
  prettyPrec' = \case
    Var x -> pure $ annotate AName $ name x
    App f s ->
      parensP PP.FunArg $
      PP.hangsA 2 (pp1 f') (map pp1 $ ss ++ [s])
      where
        (f', ss) = getApps f
        pp1 = PP.withPrec PP.FunArg . PP.prettyPrec'
    Ann π s a ℓ -> fmap parens $
      PP.hangsA 2
        (PP.hsepA [ppUsage π, pure pipe, ppOuter s])
        [ PP.hsepA [pure colon, ppOuter a],
          PP.hsepA [pure colon, ppOuter (Star ℓ :: Term primTy primVal)]
        ]


class ToPPAnn ann where
  toPPAnn :: ann -> PPAnn

instance ToPPAnn () where
  toPPAnn () = Last Nothing

instance ToPPAnn PPAnn where
  toPPAnn = identity

instance ToPPAnn Nat.PPAnn where
  toPPAnn = fmap \case
    Nat.Lit -> APrimVal
    Nat.Fun -> APrimFun
    Nat.Paren -> APunct
