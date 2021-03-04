-- |
-- - Order of Passes
--   1. =RemoveModule=
--   2. =RemoveHandler=
--   3. =RemoveGuard=
--   4. =RemoveCond=
--   5. =CombineMultiple=
--   6. =RemoveSignature=
--   7. =RemovePunned=
--   8. =RemoveDo=
module Juvix.FrontendDesugar where

import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.FrontendDesugar.CombineMultiple.Transform as Multiple
import qualified Juvix.FrontendDesugar.RemoveCond.Transform as Cond
import qualified Juvix.FrontendDesugar.RemoveDo.Transform as Do
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
import qualified Juvix.FrontendDesugar.RemoveGuard.Transform as Guard
import qualified Juvix.FrontendDesugar.RemoveHandlers.Transform as Handler
import qualified Juvix.FrontendDesugar.RemoveModules.Transform as Module
import qualified Juvix.FrontendDesugar.RemovePunned.Transform as Punned
import qualified Juvix.FrontendDesugar.RemoveSignature.Transform as Signature
import Juvix.Library

op :: [Initial.TopLevel] -> [Target.TopLevel]
op = desugar

desugar :: [Initial.TopLevel] -> [Target.TopLevel]
desugar xs =
  xs
    |> fmap (Cond.transformTopLevel . Handler.transformTopLevel . Guard.transformTopLevel . Module.transformTopLevel)
    |> Multiple.transformTopLevel
    |> Signature.transformTopLevel
    |> fmap (Do.transformTopLevel . Punned.transformTopLevel)
