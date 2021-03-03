{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/heliax/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types.Base where

import Control.Lens
import qualified Data.Data as D
import Extensible
import Juvix.Library hiding (Product, Sum, Type)
import qualified Juvix.Library.Usage as Usage

type ConstructorName = NameSymb

type NameSymb = NonEmpty Symbol

type ModuleName = NameSymb

extensible
  [d|
    -- we will want an include... but this will have to be deferred to a context phase
    data TopLevel
      = Type Type
      | ModuleOpen ModuleOpen
      | Signature Signature
      | Module Module
      | Function Function
      | Handler Handler
      | Declaration Declaration
      | TypeClass
      | TypeClassInstance
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------------------------------------
    -- Declarations
    --------------------------------------------------------------------------------
    data Declaration
      = Infixivity InfixDeclar
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data InfixDeclar
      = NonAssoc Symbol Natural
      | AssocL Symbol Natural
      | AssocR Symbol Natural
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------------------------------------
    -- Types
    --------------------------------------------------------------------------------

    data Type
      = Typ
          -- Was a usage but can't alias for now
          { typeUsage :: Maybe Expression,
            typeName' :: !Symbol,
            typeArgs :: [Symbol],
            typeForm :: Data
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- 'Data' is the data declaration in the Juvix language
    data Data
      = Arrowed
          { dataArrow :: Expression,
            dataAdt' :: Adt
          }
      | NonArrowed
          { dataAdt :: Adt
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------
    -- Arrows
    --------------------------------------------------
    data NamedType
      = NamedType'
          { nameRefineName :: !Name,
            namedRefineRefine :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- TODO ∷ change TypeName to TypeNameModule
    data TypeRefine
      = TypeRefine
          { typeRefineName :: Expression,
            typeRefineRefinement :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------
    -- Types Misc
    --------------------------------------------------

    data Name
      = Implicit !Symbol
      | Concrete !Symbol
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data ArrowSymbol
      = ArrowUse Usage.T
      | -- Was a usage but can't alias for now
        ArrowExp Expression
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- Was a new type
    -- TODO ∷ finish this type!
    data UniverseExpression
      = UniverseExpression Symbol
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------
    -- ADTs
    --------------------------------------------------

    -- The types for ADT are not the most
    -- constraining, however are setup to have the
    -- least amount of boilerplate in the latter
    -- stages as possible

    -- a Sum of length one should not have a record
    -- [type Address = Foo {abc : Int}]
    -- this form should be considered illegal unless we wish to permit
    -- named records along with unnamed records.
    -- Ι suspect in the future this will instead be used for Enum
    -- Subsets with refined information
    data Adt
      = Sum (NonEmpty Sum)
      | Product Product
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Sum
      = S
          { sumConstructor :: !Symbol,
            sumValue :: !(Maybe Product)
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- for when a product is without a sum
    -- only a record can apply
    -- a sum of only one is a named product
    data Product
      = Record !Record
      | Arrow Expression
      | ADTLike [Expression]
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Record
      = Record''
          { recordFields :: NonEmpty NameType,
            recordFamilySignature :: Maybe Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data NameType
      = NameType'
          { nameTypeSignature :: Expression,
            nameTypeName :: !Name
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------------------------------------
    -- Functions And Modules
    --------------------------------------------------------------------------------

    -- was a newtype

    -- 'Function' is a normal signature with a name arguments and a body
    -- that may or may not have a guard before it
    data Function
      = Func (FunctionLike Expression)
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

   -- 'Handler' is just like a regular function, but it is meant to also
   -- contain perform effects. Handlers are meant to be used with Witch
   -- primarily
   -- However, Handlers are NOT scoped, meaning that they can't be defined
   -- defined within another function. We CAN fix that, but it requires
   -- us to make some choices, it's wise to have Witch up and running first.
    data Handler
      = Hand (FunctionLike Expression)
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- 'Module' is like function, however it allows multiple top levels
    data Module
      = Mod (FunctionLike (NonEmpty TopLevel))
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data ModuleE
      = ModE
          { moduleEBindings :: FunctionLike (NonEmpty TopLevel),
            moduleEBody :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- 'FunctionLike' is the generic version for modules, handlers and functions
    data FunctionLike a
      = Like
          { functionLikedName :: Symbol,
            functionLikeArgs :: [Arg],
            functionLikeBody :: GuardBody a
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- 'GuardBody' determines if a form is a guard or a body
    data GuardBody a
      = Body a
      | Guard (Cond a)
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data ModuleOpen
      = Open ModuleName
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data ModuleOpenExpr
      = OpenExpress
          { moduleOpenExprModuleN :: ModuleName,
            moduleOpenExprExpr :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- Very similar to name, but match instead of symbol
    data Arg
      = ImplicitA MatchLogic
      | ConcreteA MatchLogic
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Cond a
      = C (NonEmpty (CondLogic a))
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data CondLogic a
      = CondExpression
          { condLogicPred :: Expression,
            condLogicBody :: a
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------------------------------------
    -- Signatures
    --------------------------------------------------------------------------------

    data Signature
      = Sig
          { signatureName :: Symbol,
            -- Was a usage but can't alias for now
            signatureUsage :: Maybe Expression,
            signatureArrowType :: Expression,
            signatureConstraints :: [Expression]
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------------------------------------
    -- Type Classes
    --------------------------------------------------------------------------------

    --------------------------------------------------------------------------------
    -- Expression
    --------------------------------------------------------------------------------

    -- TODO ∷ add <expression> : <expression> <refine>?
    -- to the parser
    data Expression
      = Cond (Cond Expression)
      | Constant Constant
      | ModuleE ModuleE
      | Let Let
      | LetType LetType
      | Match Match
      | Name NameSymb
      | OpenExpr ModuleOpenExpr
      | Lambda Lambda
      | Application Application
      | Primitive Primitive
      | List List
      | Tuple Tuple
      | Block Block
      | Infix Infix
      | ExpRecord ExpRecord
      | Do Do
      | -- Added due to merge
        ArrowE ArrowExp
      | NamedTypeE NamedType
      | RefinedE TypeRefine
      | UniverseName UniverseExpression
      | Parened Expression
      | DeclarationE DeclarationExpression
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data DeclarationExpression
      = DeclareExpression Declaration Expression
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Primitive
      = Prim NameSymb
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data List
      = ListLit [Expression]
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Tuple
      = TupleLit [Expression]
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data ArrowExp
      = Arr'
          { arrowExpLeft :: Expression,
            -- Was a usage but can't alias for now
            arrowExpUsage :: Expression,
            arrowExpRight :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Constant
      = Number Numb
      | String String'
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Numb
      = Integer' Integer
      | Double' Double
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data String'
      = Sho Text
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Block
      = Bloc
          {blockExpr :: Expression}
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Lambda
      = Lamb
          { lambdaArgs :: NonEmpty MatchLogic,
            lambdaBody :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data Application
      = App
          { applicationName :: Expression,
            applicationArgs :: NonEmpty Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- Was a newtype but extensible adds fields
    data Do
      = Do'' (NonEmpty DoBody)
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- promote this to a match!!!
    data DoBody
      = DoBody
          { doBodyName :: Maybe Symbol,
            doBodyExpr :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- TODO ∷ we need includes in here as well!
    -- Was a newtype but extensible adds fields
    data ExpRecord
      = ExpressionRecord
          { expRecordFields :: NonEmpty (NameSet Expression)
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------
    -- Symbol Binding
    --------------------------------------------------

    data Let
      = Let''
          { letBindings :: FunctionLike Expression,
            letBody :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data LetType
      = LetType''
          { letTypeBindings :: Type,
            letTypeBody :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- TODO ∷ have letSig

    --------------------------------------------------
    -- Symbol Binding
    --------------------------------------------------

    data Infix
      = Inf
          { infixLeft :: Expression,
            infixOp :: NameSymb,
            infixRight :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    --------------------------------------------------
    -- Matching
    --------------------------------------------------

    data Match
      = Match''
          { matchOn :: Expression,
            matchBindigns :: NonEmpty MatchL
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data MatchL
      = MatchL
          { matchLPattern :: MatchLogic,
            matchLBody :: Expression
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    -- TODO ∷ add literals to the match
    data MatchLogic
      = MatchLogic
          { matchLogicContents :: MatchLogicStart,
            matchLogicNamed :: Maybe Symbol
          }
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data MatchLogicStart
      = MatchCon ConstructorName [MatchLogic]
      | MatchName Symbol
      | MatchConst Constant
      | MatchRecord (NonEmpty (NameSet MatchLogic))
      deriving (Show, Read, Generic, NFData, D.Data, Eq)

    data NameSet t
      = Punned NameSymb
      | NonPunned NameSymb t
      deriving (Show, Read, Generic, NFData, D.Data, Eq)
    |]

data Header topLevel
  = Header NameSymb [topLevel]
  | NoHeader [topLevel]
  deriving (Show, Read, Generic, NFData, D.Data, Eq)

--------------------------------------------------------------------------------
-- Lens creation
--------------------------------------------------------------------------------

makeLensesWith camelCaseFields ''Data'

makeLensesWith camelCaseFields ''Type'

makeLensesWith camelCaseFields ''Sum'

makeLensesWith camelCaseFields ''Record'

makeLensesWith camelCaseFields ''NamedType'

makeLensesWith camelCaseFields ''TypeRefine'

makeLensesWith camelCaseFields ''CondLogic'

makeLensesWith camelCaseFields ''Let'

makeLensesWith camelCaseFields ''Match'

makeLensesWith camelCaseFields ''MatchL'

makeLensesWith camelCaseFields ''MatchLogic'

makeLensesWith camelCaseFields ''FunctionLike'

makeLensesWith camelCaseFields ''Module'

makeLensesWith camelCaseFields ''Function'

makeLensesWith camelCaseFields ''Handler'

makeLensesWith camelCaseFields ''Lambda'

makeLensesWith camelCaseFields ''Application'

makeLensesWith camelCaseFields ''Signature'

makeLensesWith camelCaseFields ''Block'

makeLensesWith camelCaseFields ''Do'

makeLensesWith camelCaseFields ''DoBody'

makeLensesWith camelCaseFields ''ModuleOpenExpr'

makeLensesWith camelCaseFields ''Infix'

makeLensesWith camelCaseFields ''ExpRecord'
