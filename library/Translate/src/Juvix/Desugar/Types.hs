module Juvix.Desugar.Types where

import qualified Juvix.Desugar.Extend as Ext
import Juvix.Frontend.Types.Base
import Juvix.Library hiding (Product, Sum)

data T

extendType "Type" [] [t|T|] Ext.extendType

extendTopLevel "TopLevel" [] [t|T|] Ext.extendTopLevel

extendDeclaration "Declaration" [] [t|T|] Ext.extendDeclaration

extendDeclarationExpression "DeclarationExpression" [] [t|T|] Ext.extendDeclarationExpression

extendInfixDeclar "InfixDeclar" [] [t|T|] Ext.extendInfixDeclar

extendData "Data" [] [t|T|] Ext.extendData

extendList "List" [] [t|T|] Ext.extendList

extendTuple "Tuple" [] [t|T|] Ext.extendTuple

extendPrimitive "Primitive" [] [t|T|] Ext.extendPrimitive

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

extendFunction "Function" [] [t|T|] $ Ext.extendFunction [t|T|]

extendArg "Arg" [] [t|T|] Ext.extendArg

extendFunctionLike "FunctionLike" [] [t|T|] $ Ext.extendFunctionLike [t|T|]

extendModuleOpen "ModuleOpen" [] [t|T|] Ext.extendModuleOpen

extendModuleOpenExpr "ModuleOpenExpr" [] [t|T|] Ext.extendModuleOpenExpr

extendExpression "Expression" [] [t|T|] Ext.extendExpression

extendArrowExp "ArrowExp" [] [t|T|] Ext.extendArrowExp

extendConstant "Constant" [] [t|T|] Ext.extendConstant

extendNumb "Numb" [] [t|T|] Ext.extendNumb

extendString' "String'" [] [t|T|] Ext.extendString'

extendBlock "Block" [] [t|T|] Ext.extendBlock

extendLambda "Lambda" [] [t|T|] Ext.extendLambda

extendApplication "Application" [] [t|T|] Ext.extendApplication

extendExpRecord "ExpRecord" [] [t|T|] Ext.extendExpRecord

extendLet "Let" [] [t|T|] $ Ext.extendLet [t|T|]

extendLetType "LetType" [] [t|T|] Ext.extendLetType

extendInfix "Infix" [] [t|T|] Ext.extendInfix

extendMatch "Match" [] [t|T|] Ext.extendMatch

extendMatchL "MatchL" [] [t|T|] Ext.extendMatchL

extendMatchLogic "MatchLogic" [] [t|T|] Ext.extendMatchLogic

extendMatchLogicStart "MatchLogicStart" [] [t|T|] Ext.extendMatchLogicStart

extendNameSet "NameSet" [] [t|T|] $ const Ext.extendNameSet

--------------------------------------------------------------------------------
-- Instantiating for show derivation
--------------------------------------------------------------------------------

extendCond "Cond" [] [t|T|] $ const Ext.extendCond

extendCondLogic "CondLogic" [] [t|T|] $ const Ext.extendCondLogic

extendModule "Module" [] [t|T|] Ext.extendModule

extendSignature "Signature" [] [t|T|] Ext.extendSignature

extendDo "Do" [] [t|T|] Ext.extendDo

extendDoBody "DoBody" [] [t|T|] Ext.extendDoBody

extendGuardBody "GuardBody" [] [t|T|] $ const Ext.extendGuardBody

extendModuleE "ModulE" [] [t|T|] Ext.extendModuleE
