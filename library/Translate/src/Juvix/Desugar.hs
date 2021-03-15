-- | Desugar takes the frontend syntax and through the =desugar=
-- function, removes all extra parts of syntax that can be boiled down
-- to simple macro expansion (simplification of the frontend syntax
-- from the syntax alone with no extra information needed!)
module Juvix.Desugar
  ( op,
  )
where

import qualified Juvix.Desugar.Passes as Pass
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp

-- | @op@ fully desugares the frontend syntax from the original
-- frontend sexp representation to a form without modules, conditions,
-- guards, etc. This pass thus does all transformations that do not
-- requires a context
op :: [Sexp.T] -> [Sexp.T]
op syn =
  syn
    >>| Pass.moduleTransform
    >>| Pass.moduleLetTransform
    >>| Pass.condTransform
    >>| Pass.ifTransform
    >>| Pass.multipleTransLet
    |> Pass.multipleTransDefun
    |> Pass.combineSig
    >>| Pass.removePunnedRecords
    >>| Pass.translateDo
