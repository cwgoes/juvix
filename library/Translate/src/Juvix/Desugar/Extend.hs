-- |
-- - This Desugars the frontend syntax
--   + Belongs to Table
--     | Changed      | Is a Sum Type of |
--     |--------------+------------------|
--     | Do           | Expression       |
--     | ModuleE      | Expression       |
--     | Module       | TopLevel         |
--     | Function     | TopLevel         |
--     | Signature    | TopLevel         |
--     | Let          | Let              |
--     | FunctionLike | Function ∧ Let   |
-- - Thus one does not have to ever deal with
-- - _Expression_
--   + one does not have to deal with the following Sum Constructors
--   #+begin_src haskell
--     data Do
--       = Do'' (NonEmpty DoBody)
--       deriving (Show, Generic, NFData)
--
--     data DoBody
--       = DoBody
--         { doBodyName :: Maybe NameSymb,
--           doBodyExpr :: Expression
--         }
--       deriving (Show, Generic, NFData)
--
--     data ModuleE
--       = ModE
--           { moduleEBindings :: FunctionLike (NonEmpty TopLevel)
--           , moduleEBody :: Expression
--           }
--       deriving (Show, Generic, NFData)
--
--     data Cond a
--       = C (NonEmpty (CondLogic a))
--       deriving (Show, Generic, NFData)
--
--     data CondLogic a
--       = CondExpression
--           { condLogicPred :: Expression
--           , condLogicBody :: a
--           }
--       deriving (Show, Generic, NFData)
--   #+end_src
-- - _Top Level_
--   + one does not have to deal with the following Sum Constructors
--    #+begin_src haskell
--     data Module
--       = Mod (FunctionLike (NonEmpty TopLevel))
--       deriving (Show, Generic, NFData)
--
--       data Signature
--         = Sig
--             { signatureName :: Symbol
--               -- Was a usage but can't alias for now
--             , signatureUsage :: Maybe Expression
--             , signatureArrowType :: Expression
--             , signatureConstraints :: [Expression]
--             }
--         deriving (Show, Generic, NFData)
--   #+end_src
-- - _Function_
--   + This form now looks like
--     #+begin_src haskell
--       data Function
--         = Func Symbol (NonEmpty (FunctionLike Expression)) (Maybe Signature)
--     #+end_src
--
-- - _NameSet_
--   + This form now looks like
--     #+begin_src haskell
--     data NameSet t
--       = NonPunned NameSymb t
--       deriving (Show, Generic, NFData)
--     #+end_src
module Juvix.Desugar.Extend
  ( module Juvix.Desugar.Extend,
    module Juvix.Frontend.Types.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.Frontend.Types.Extend hiding
  ( extendExpression,
    extendFunction,
    extendFunctionLike,
    extendLet,
    extendNameSet,
    extendTopLevel,
  )
import Juvix.Library

extendExpression :: ExtExpression
extendExpression =
  removeCond defaultExtExpression
    |> removeDo
    |> removeModuleE

extendTopLevel :: ExtTopLevel
extendTopLevel = defaultExtTopLevel {typeModule = Nothing}

extendLet :: Extension.TypeQ -> ExtLet
extendLet = removeLetName

extendFunctionLike :: Extension.TypeQ -> Extension.TypeQ -> ExtFunctionLike
extendFunctionLike = functionLikeNoNameCond

extendFunction :: Extension.TypeQ -> ExtFunction
extendFunction = functionNoSignature

extendNameSet :: ExtNameSet
extendNameSet = removeNameSet

--------------------------------------------------------------------------------
-- Individual passes
--------------------------------------------------------------------------------

removeDo :: ExtExpression -> ExtExpression
removeDo ext = ext {typeDo = Nothing}

removeModuleE :: ExtExpression -> ExtExpression
removeModuleE ext = ext {typeModuleE = Nothing}

removeModule :: ExtTopLevel -> ExtTopLevel
removeModule ext = ext {typeModule = Nothing}

removeCond :: ExtExpression -> ExtExpression
removeCond ext = ext {typeCond = Nothing}

removeNameSet :: ExtNameSet
removeNameSet = defaultExtNameSet {typePunned = Nothing}

removeLetName :: Extension.TypeQ -> ExtLet
removeLetName arg =
  defaultExtLet
    { typeLet'' = Nothing,
      typeLetX =
        [ ( "LetGroup",
            [ ("letName", [t|Symbol|]),
              ("letBindings", [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|]),
              ("letBody", [t|Expression' $arg|])
            ]
          )
        ]
    }

functionLikeNoNameCond :: Extension.TypeQ -> Extension.TypeQ -> ExtFunctionLike
functionLikeNoNameCond arg a =
  defaultExtFunctionLike
    { typeLike = Nothing,
      typeFunctionLikeX =
        [ ( "Like",
            [ ("functionLikeArgs", [t|[Arg' $arg]|]),
              ("functionLikeBody", a)
            ]
          )
        ]
    }

functionNoSignature :: Extension.TypeQ -> ExtFunction
functionNoSignature arg =
  -- have to use the default here as we have a function in the previous code
  defaultExtFunction
    { typeFunc = Nothing,
      typeFunctionX =
        [ ( "Func",
            [ [t|Symbol|],
              [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|],
              [t|Maybe (Signature' $arg)|]
            ]
          )
        ]
    }
