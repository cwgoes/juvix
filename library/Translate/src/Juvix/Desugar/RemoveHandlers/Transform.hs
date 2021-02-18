module Juvix.FrontendDesugar.RemoveHandlers.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Frontend.Types as Old
import qualified Juvix.FrontendDesugar.RemoveHandlers.Types as New
import Juvix.Library

transformHandler :: Old.Handler -> New.Expression
transformHandler (Old.Handler (Old.Like name args body) restExpr) =
  transformGuardBody transformTop body
    |> New.Like name (transformArg <$> args)
    |> flip New.Let'' (transformExpression restExpr)
    |> New.Let

transformTop ::
  NonEmpty Old.TopLevel -> New.Expression
transformTop body = undefined

transformFunctionLike ::
  Old.FunctionLike Old.Expression -> New.FunctionLike New.Expression
transformFunctionLike (Old.Like name args body) =
  New.Like name (transformArg <$> args) (transformGuardBody transformExpression body)

transformGuardBody :: (a1 -> a2) -> Old.GuardBody a1 -> New.GuardBody a2
transformGuardBody guardF (Old.Body x) = New.Body (guardF x)
transformGuardBody guardF (Old.Guard x) = New.Guard (transformCond guardF x)

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------
transformTopLevel :: Old.TopLevel -> New.TopLevel
transformTopLevel (Old.Type t) =
  New.Type (transformType t)
transformTopLevel (Old.ModuleOpen t) =
  New.ModuleOpen (transformModuleOpen t)
transformTopLevel (Old.Signature t) =
  New.Signature (transformSignature t)
transformTopLevel (Old.Function t) =
  New.Function (transformFunction t)
transformTopLevel (Old.Module m) =
  transformModule m
transformTopLevel (Old.Declaration i) =
  New.Declaration (transformDeclaration i)
transformTopLevel Old.TypeClass =
  New.TypeClass
transformTopLevel Old.TypeClassInstance =
  New.TypeClassInstance

transformExpression :: Old.Expression -> New.Expression
transformExpression (Old.Tuple t) =
  New.Tuple (transformTuple t)
transformExpression (Old.List t) =
  New.List (transformList t)
transformExpression (Old.Primitive t) =
  New.Primitive (transformPrim t)
transformExpression (Old.Cond c) =
  New.Cond (transformCond transformExpression c)
transformExpression (Old.ModuleE m) =
  transformModuleE m
transformExpression (Old.Constant c) =
  New.Constant (transformConst c)
transformExpression (Old.Let l) =
  New.Let (transformLet l)
transformExpression (Old.LetType l) =
  New.LetType (transformLetType l)
transformExpression (Old.Match m) =
  New.Match (transformMatch m)
transformExpression (Old.Name n) =
  New.Name n
transformExpression (Old.OpenExpr n) =
  New.OpenExpr (transformModuleOpenExpr n)
transformExpression (Old.Lambda l) =
  New.Lambda (transformLambda l)
transformExpression (Old.Application a) =
  New.Application (transformApplication a)
transformExpression (Old.Block b) =
  New.Block (transformBlock b)
transformExpression (Old.Infix i) =
  New.Infix (transformInfix i)
transformExpression (Old.ExpRecord i) =
  New.ExpRecord (transformExpRecord i)
transformExpression (Old.Do i) =
  New.Do (transformDo i)
transformExpression (Old.ArrowE i) =
  New.ArrowE (transformArrowExp i)
transformExpression (Old.NamedTypeE i) =
  New.NamedTypeE (transformNamedType i)
transformExpression (Old.RefinedE i) =
  New.RefinedE (transformTypeRefine i)
transformExpression (Old.UniverseName i) =
  New.UniverseName (transformUniverseExpression i)
transformExpression (Old.Parened e) =
  New.Parened (transformExpression e)
transformExpression (Old.DeclarationE e) =
  New.DeclarationE (transformDeclarationExpression e)

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

transformDeclarationExpression ::
  Old.DeclarationExpression -> New.DeclarationExpression
transformDeclarationExpression (Old.DeclareExpression i e) =
  New.DeclareExpression (transformDeclaration i) (transformExpression e)

transformDeclaration :: Old.Declaration -> New.Declaration
transformDeclaration (Old.Infixivity i) =
  New.Infixivity (transformInfixDeclar i)

transformInfixDeclar :: Old.InfixDeclar -> New.InfixDeclar
transformInfixDeclar (Old.AssocL n i) = New.AssocL n i
transformInfixDeclar (Old.AssocR n i) = New.AssocR n i
transformInfixDeclar (Old.NonAssoc n i) = New.NonAssoc n i

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType :: Old.Type -> New.Type
transformType (Old.Typ usage name' args form) =
  New.Typ (transformExpression <$> usage) name' args (transformData form)

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType :: Old.NamedType -> New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' (transformName name) (transformExpression exp)

transformTypeRefine :: Old.TypeRefine -> New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine (transformExpression name) (transformExpression refine)

--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName :: Old.Name -> New.Name
transformName (Old.Implicit s) = New.Implicit s
transformName (Old.Concrete s) = New.Concrete s

transformArrowSymbol :: Old.ArrowSymbol -> New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp (transformExpression e)

transformUniverseExpression ::
  Old.UniverseExpression -> New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData :: Old.Data -> New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed (transformExpression exp) (transformAdt adt)
transformData (Old.NonArrowed adt) =
  New.NonArrowed (transformAdt adt)

transformAdt :: Old.Adt -> New.Adt
transformAdt (Old.Sum oldsu) = New.Sum (transformSum <$> oldsu)
transformAdt (Old.Product p) = New.Product (transformProduct p)

transformSum :: Old.Sum -> New.Sum
transformSum (Old.S sym prod) =
  New.S sym (transformProduct <$> prod)

transformProduct :: Old.Product -> New.Product
transformProduct (Old.Record rec') = New.Record (transformRecord rec')
transformProduct (Old.Arrow arrow) = New.Arrow (transformExpression arrow)
transformProduct (Old.ADTLike adt) = New.ADTLike (transformExpression <$> adt)

transformRecord :: Old.Record -> New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' (transformNameType <$> fields) (transformExpression <$> sig)

transformNameType :: Old.NameType -> New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' (transformExpression sig) (transformName name)

--------------------------------------------------------------------------------
-- Functions And Modules
--------------------------------------------------------------------------------

transformFunction :: Old.Function -> New.Function
transformFunction (Old.Func f) = New.Func (transformFunctionLike f)

transformModuleOpen :: Old.ModuleOpen -> New.ModuleOpen
transformModuleOpen (Old.Open mod) = New.Open mod

transformModuleOpenExpr :: Old.ModuleOpenExpr -> New.ModuleOpenExpr
transformModuleOpenExpr (Old.OpenExpress modName expr) =
  New.OpenExpress modName (transformExpression expr)

transformArg :: Old.Arg -> New.Arg
transformArg (Old.ImplicitA ml) = New.ImplicitA (transformMatchLogic ml)
transformArg (Old.ConcreteA ml) = New.ConcreteA (transformMatchLogic ml)

transformCond :: (t -> a) -> Old.Cond t -> New.Cond a
transformCond trans (Old.C logs) = Old.C (transformCondLogic trans <$> logs)

transformCondLogic :: (t -> a) -> Old.CondLogic t -> New.CondLogic a
transformCondLogic trans (Old.CondExpression p b) =
  New.CondExpression (transformExpression p) (trans b)

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

transformSignature :: Old.Signature -> New.Signature
transformSignature (Old.Sig name usage arrow constraints) =
  New.Sig
    name
    (transformExpression <$> usage)
    (transformExpression arrow)
    (transformExpression <$> constraints)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

transformArrowExp :: Old.ArrowExp -> New.ArrowExp
transformArrowExp (Old.Arr' left usage right) =
  New.Arr'
    (transformExpression left)
    (transformExpression usage)
    (transformExpression right)

transformPrim :: Old.Primitive -> New.Primitive
transformPrim (Old.Prim p) = New.Prim p

transformTuple :: Old.Tuple -> New.Tuple
transformTuple (Old.TupleLit t) = New.TupleLit (transformExpression <$> t)

transformList :: Old.List -> New.List
transformList (Old.ListLit t) = New.ListLit (transformExpression <$> t)

transformConst :: Old.Constant -> New.Constant
transformConst (Old.Number numb) = New.Number (transformNumb numb)
transformConst (Old.String str) = New.String (transformString str)

transformNumb :: Old.Numb -> New.Numb
transformNumb (Old.Integer' i) = New.Integer' i
transformNumb (Old.Double' d) = New.Double' d

transformString :: Old.String' -> New.String'
transformString (Old.Sho t) = New.Sho t

transformBlock :: Old.Block -> New.Block
transformBlock (Old.Bloc expr) = New.Bloc (transformExpression expr)

transformLambda :: Old.Lambda -> New.Lambda
transformLambda (Old.Lamb args body) =
  New.Lamb (transformMatchLogic <$> args) (transformExpression body)

transformApplication :: Old.Application -> New.Application
transformApplication (Old.App fun args) =
  New.App (transformExpression fun) (transformExpression <$> args)

transformDo :: Old.Do -> New.Do
transformDo (Old.Do'' dos) = New.Do'' (transformDoBody <$> dos)

transformDoBody :: Old.DoBody -> New.DoBody
transformDoBody (Old.DoBody name expr) =
  New.DoBody name (transformExpression expr)

transformExpRecord :: Old.ExpRecord -> New.ExpRecord
transformExpRecord (Old.ExpressionRecord fields) =
  New.ExpressionRecord (transformNameSet transformExpression <$> fields)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformLet :: Old.Let -> New.Let
transformLet (Old.Let'' bindings body) =
  New.Let'' (transformFunctionLike bindings) (transformExpression body)

transformLetType :: Old.LetType -> New.LetType
transformLetType (Old.LetType'' typ expr) =
  New.LetType'' (transformType typ) (transformExpression expr)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

transformInfix :: Old.Infix -> New.Infix
transformInfix (Old.Inf l o r) =
  New.Inf (transformExpression l) o (transformExpression r)

--------------------------------------------------
-- Matching
--------------------------------------------------

transformMatch :: Old.Match -> New.Match
transformMatch (Old.Match'' on bindings) =
  New.Match'' (transformExpression on) (transformMatchL <$> bindings)

transformMatchL :: Old.MatchL -> New.MatchL
transformMatchL (Old.MatchL pat body) =
  New.MatchL (transformMatchLogic pat) (transformExpression body)

transformMatchLogic :: Old.MatchLogic -> New.MatchLogic
transformMatchLogic (Old.MatchLogic start name) =
  New.MatchLogic (tranformMatchLogicStart start) name

tranformMatchLogicStart :: Old.MatchLogicStart -> New.MatchLogicStart
tranformMatchLogicStart (Old.MatchCon conName logic) =
  New.MatchCon conName (transformMatchLogic <$> logic)
tranformMatchLogicStart (Old.MatchName s) =
  New.MatchName s
tranformMatchLogicStart (Old.MatchConst c) =
  New.MatchConst (transformConst c)
tranformMatchLogicStart (Old.MatchRecord r) =
  New.MatchRecord (transformNameSet transformMatchLogic <$> r)

transformNameSet :: (t -> t1) -> Old.NameSet t -> New.NameSet t1
transformNameSet _ (Old.Punned s) =
  New.Punned s
transformNameSet p (Old.NonPunned s e) =
  New.NonPunned s (p e)
