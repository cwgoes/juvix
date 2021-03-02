module Juvix.FrontendDesugar.RemoveHandlers.Extend
  ( module Juvix.FrontendDesugar.RemoveGuard.Extend,
    module Juvix.FrontendDesugar.RemoveHandlers.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.RemoveGuard.Extend hiding (extendTopLevel)
import qualified Juvix.FrontendDesugar.RemoveGuard.Extend as Ext
import Juvix.Library

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeHandler = Nothing}
