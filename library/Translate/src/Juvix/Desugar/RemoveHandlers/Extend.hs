module Juvix.FrontendDesugar.RemoveHandlers.Extend
  ( module Juvix.Frontend.Types.Extend,
    module Juvix.FrontendDesugar.RemoveModules.Extend,
  )
where

import Juvix.Frontend.Types.Base
import Juvix.Frontend.Types.Extend hiding (extendExpression, extendTopLevel)
import qualified Juvix.Frontend.Types.Extend as Ext
import Juvix.Library hiding (Product, Sum)

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeHandlerE = Nothing}

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeHandler = Nothing}
