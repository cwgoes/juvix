-- |
-- - Order of Passes
--   1. =RemoveModule=
--   2. =RemoveGuard=
--   3. =RemoveCond=
--   4. =CombineMultiple=
--   5. =RemoveSignature=
--   6. =RemovePunned=
--   7. =RemoveDo=
module Juvix.FrontendDesugar where

import Juvix.Conversion.ML as ML
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Desugar.Types as Target
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types as Initial
import Juvix.Library

op :: [Initial.TopLevel] -> [Target.TopLevel]
op = fmap ML.op . Desugar.op . fmap SexpTrans.transTopLevel
