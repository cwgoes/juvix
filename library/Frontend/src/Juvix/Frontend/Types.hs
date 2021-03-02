{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/cryptiumlabs/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types
  ( module Juvix.Frontend.Types,
    Header (..),
  )
where

import Juvix.Frontend.Types.Base
import qualified Juvix.Frontend.Types.Extend as Ext
import Juvix.Library hiding (Product, Sum)

-- Unwrap the header from the rest of the definitions
extractTopLevel :: Header topLevel -> [topLevel]
extractTopLevel (Header _ tops) = tops
extractTopLevel (NoHeader tops) = tops

data T

extendType "Type" [] [t|T|] Ext.extendType

extendTopLevel "TopLevel" [] [t|T|] Ext.extendTopLevel

extendDeclaration "Declaration" [] [t|T|] Ext.extendDeclaration

extendDeclarationExpression "DeclarationExpression" [] [t|T|] Ext.extendDeclarationExpression

extendInfixDeclar "InfixDeclar" [] [t|T|] Ext.extendInfixDeclar

extendData "Data" [] [t|T|] Ext.extendData

extendNamedType "NamedType" [] [t|T|] Ext.extendNamedType

extendTypeRefine "TypeRefine" [] [t|T|] Ext.extendTypeRefine

extendName "Name" [] [t|T|] Ext.extendName

extendArrowSymbol "ArrowSymbol" [] [t|T|] Ext.extendArrowSymbol

extendUniverseExpression "UniverseExpression" [] [t|T|] Ext.extendUniverseExpression

extendAdt "Adt" [] [t|T|] Ext.extendAdt

extendSum "Sum" [] [t|T|] Ext.extendSum

extendProduct "Product" [] [t|T|] Ext.extendProduct

extendRecord "Record" [] [t|T|] Ext.extendRecord

extendNameType "NameType" [] [t|T|] Ext.extendNameType

extendFunction "Function" [] [t|T|] Ext.extendFunction

extendHandler "Handler" [] [t|T|] Ext.extendHandler

extendModule "Module" [] [t|T|] Ext.extendModule

extendModuleE "ModuleE" [] [t|T|] Ext.extendModuleE

extendFunctionLike "FunctionLike" [] [t|T|] $ const Ext.extendFunctionLike

extendGuardBody "GuardBody" [] [t|T|] $ const Ext.extendGuardBody

extendModuleOpen "ModuleOpen" [] [t|T|] Ext.extendModuleOpen

extendModuleOpenExpr "ModuleOpenExpr" [] [t|T|] Ext.extendModuleOpenExpr

extendArg "Arg" [] [t|T|] Ext.extendArg

extendCond "Cond" [] [t|T|] $ const Ext.extendCond

extendCondLogic "CondLogic" [] [t|T|] $ const Ext.extendCondLogic

extendSignature "Signature" [] [t|T|] Ext.extendSignature

extendExpression "Expression" [] [t|T|] Ext.extendExpression

extendArrowExp "ArrowExp" [] [t|T|] Ext.extendArrowExp

extendList "List" [] [t|T|] Ext.extendList

extendTuple "Tuple" [] [t|T|] Ext.extendTuple

extendPrimitive "Primitive" [] [t|T|] Ext.extendPrimitive

extendConstant "Constant" [] [t|T|] Ext.extendConstant

extendNumb "Numb" [] [t|T|] Ext.extendNumb

extendString' "String'" [] [t|T|] Ext.extendString'

extendBlock "Block" [] [t|T|] Ext.extendBlock

extendLambda "Lambda" [] [t|T|] Ext.extendLambda

extendApplication "Application" [] [t|T|] Ext.extendApplication

extendDo "Do" [] [t|T|] Ext.extendDo

extendDoBody "DoBody" [] [t|T|] Ext.extendDoBody

extendExpRecord "ExpRecord" [] [t|T|] Ext.extendExpRecord

extendLet "Let" [] [t|T|] Ext.extendLet

extendLetType "LetType" [] [t|T|] Ext.extendLetType

extendInfix "Infix" [] [t|T|] Ext.extendInfix

extendMatch "Match" [] [t|T|] Ext.extendMatch

extendMatchL "MatchL" [] [t|T|] Ext.extendMatchL

extendMatchLogic "MatchLogic" [] [t|T|] Ext.extendMatchLogic

extendMatchLogicStart "MatchLogicStart" [] [t|T|] Ext.extendMatchLogicStart

extendNameSet "NameSet" [] [t|T|] $ const Ext.extendNameSet
