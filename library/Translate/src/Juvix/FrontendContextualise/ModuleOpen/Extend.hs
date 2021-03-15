-- |
-- - This pass removes the =Infix= form
--   + Belongs to Table
--     | Changed        | Is a Sum Type of |
--     |----------------+------------------|
--     | ModuleOpen     | TopLevel         |
--     | ModuleOpenExpr | Expression       |
-- - Thus one does not have to ever deal with
--   #+begin_src haskell
--     data ModuleOpen
--       = Open ModuleName
--       deriving (Show, Generic, NFData)
--
--     data ModuleOpenExpr
--       = OpenExpress
--         { moduleOpenExprModuleN :: ModuleName
--         , moduleOpenExprExpr    :: Expression }
--       deriving (Show, Generic, NFData)
--   #+end_src
-- - This pass with thus try to qualify all names as we go, turning
-- #+begin_src ocaml
--
--   let foo =
--     open Core in
--     List.map f xs
--
--   (* ====> *)
--   let foo =
--     Core.List.Map f xs
-- #+end_src
--
-- - for modules we can infer.
module Juvix.FrontendContextualise.ModuleOpen.Extend
  ( module Juvix.Desugar.Extend,
    module Juvix.FrontendContextualise.ModuleOpen.Extend,
  )
where

import Juvix.Desugar.Extend hiding (extendExpression, extendTopLevel)
import qualified Juvix.Desugar.Extend as Ext
import Juvix.Frontend.Types.Base
import Juvix.Library

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeModuleOpen = Nothing}

extendExpression :: ExtExpression
extendExpression = Ext.extendExpression {typeOpenExpr = Nothing}
