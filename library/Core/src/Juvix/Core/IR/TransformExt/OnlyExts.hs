-- | A transformation that discards all annotations on term/elim nodes, but
-- keeps the extensions.
module Juvix.Core.IR.TransformExt.OnlyExts where

import Extensible
import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library

data T (ext :: *)

do
  ext' <- newName "ext"
  let ext = varT ext'
  decsT <- IR.extendTerm
    "Term"
    [ext']
    [t|T $ext|]
    \primTy primVal ->
      IR.defaultExtTerm
        { IR.typeTermX = [("TermX", [[t|IR.TermX $ext $primTy $primVal|]])]
        }
  decsE <- IR.extendElim
    "Elim"
    [ext']
    [t|T $ext|]
    \primTy primVal ->
      IR.defaultExtElim
        { IR.typeElimX = [("ElimX", [[t|IR.ElimX $ext $primTy $primVal|]])]
        }
  pure $ decsT <> decsE

onlyExtsT :: IR.Term' ext primTy primVal -> IR.Term' (T ext) primTy primVal
onlyExtsT = extTransformT transformer

onlyExtsE :: IR.Elim' ext primTy primVal -> IR.Elim' (T ext) primTy primVal
onlyExtsE = extTransformE transformer

transformer :: ExtTransformTE ext (T ext) primTy primVal
transformer =
  ExtTransformTE
    { etStar = const (),
      etPrimTy = const (),
      etPrim = const (),
      etPi = const (),
      etSig = const (),
      etPair = const (),
      etUnitTy = const (),
      etUnit = const (),
      etLam = const (),
      etLet = const (),
      etElim = const (),
      etBound = const (),
      etFree = const (),
      etApp = const (),
      etAnn = const (),
      etTermX = identity,
      etElimX = identity
    }

injectT :: IR.Term primTy primVal -> IR.Term' (T ext) primTy primVal
injectT = extTransformT injector

injectE :: IR.Elim primTy primVal -> IR.Elim' (T ext) primTy primVal
injectE = extTransformE injector

injector :: ExtTransformTE IR.NoExt (T ext) primTy primVal
injector =
  ExtTransformTE
    { etStar = identity,
      etPrimTy = identity,
      etPrim = identity,
      etPi = identity,
      etSig = identity,
      etPair = identity,
      etUnitTy = identity,
      etUnit = identity,
      etLam = identity,
      etLet = identity,
      etElim = identity,
      etBound = identity,
      etFree = identity,
      etApp = identity,
      etAnn = identity,
      etTermX = absurd,
      etElimX = absurd
    }
