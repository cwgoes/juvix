-- | This module is responsible for converting back S-expressions back
-- - into the ML form. Currently this goes until the end of desugared
-- - This module is best read along side the sexpression document in
--   doc/Aricheture/Sexp.org, looking at the forms to see they are the
--   same.
--   + Note they aren't all the same, as many forms like the top level
--     let/=defun= are desugared and thus need to look at the current
--     forms of that
module Juvix.Conversion.ML
  ( op,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Desugar.Types as Target
import Juvix.Library hiding (product, sum)
import qualified Juvix.Library.NameSymbol as NameSym
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

-- | @op@ converts an s-expression at the desugar level back into ML syntax
op :: Sexp.T -> Target.TopLevel
op x
  | Sexp.isAtomNamed x ":type-class" = Target.TypeClass
  | Sexp.isAtomNamed x ":instance" = Target.TypeClassInstance
op (name Sexp.:> form)
  | named "declare" = Target.Declaration (declaration form)
  | named ":defsig-match" = Target.Function (defunSig form)
  | named "type" = Target.Type (type' form)
  | named "open" = Target.ModuleOpen (moduleOpen form)
  where
    named = Sexp.isAtomNamed name
op _ = error "malformed top level expression"

expression :: Sexp.T -> Target.Expression
expression (name Sexp.:> body)
  | named ":record-no-pun" = Target.ExpRecord (expRecord body)
  | named ":custom-arrow" = Target.ArrowE (arrowExp body)
  | named ":refinement" = Target.RefinedE (typeRefine body)
  | named ":primitive" = Target.Primitive (primitive body)
  | named ":let-match" = Target.Let (let' body)
  | named ":let-type" = Target.LetType (letType body)
  | named ":open-in" = Target.OpenExpr (openExpr body)
  | named ":declaim" = Target.DeclarationE (declarationExpression body)
  | named ":lambda" = Target.Lambda (lambda body)
  | named ":paren" = Target.Parened (parened body)
  | named ":infix" = Target.Infix (infix' body)
  | named ":tuple" = Target.Tuple (tuple body)
  | named ":progn" = Target.Block (block body)
  | named ":list" = Target.List (list' body)
  | named "case" = Target.Match (match body)
  | named ":u" = Target.UniverseName (universeExpression body)
  | otherwise = Target.Application (application (name Sexp.:> body))
  where
    named = Sexp.isAtomNamed name
expression x
  | Just a <- Sexp.atomFromT x =
    atom a
expression _ = error "malfromed expression"

atom :: Sexp.Atom -> Target.Expression
atom Sexp.A {atomName} =
  Target.Name atomName
atom Sexp.N {atomNum} =
  Target.Constant (Target.Number (Target.Integer' atomNum))

----------------------------------------------------------------------
-- Top Level Transformations
----------------------------------------------------------------------

moduleOpen :: Sexp.T -> Target.ModuleOpen
moduleOpen (Sexp.List [name])
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Target.Open atomName
moduleOpen _ = error "malformed open"

-- Takes a top level let after being transformed a few times back to
-- the ML syntax
defunSig :: Sexp.T -> Target.Function
defunSig (f Sexp.:> sig Sexp.:> forms)
  | Just fName <- eleToSymbol f,
    Just xs <- Sexp.toList forms >>= NonEmpty.nonEmpty =
    let actualSig
          | sig == Sexp.Nil = Nothing
          | otherwise =
            -- Grab usage if we appended that information
            let (usage, sigWithoutUsage) =
                  case sig of
                    Sexp.List [useName, usage, body]
                      | Sexp.isAtomNamed useName ":usage" ->
                        (Just usage, body)
                    _ -> (Nothing, sig)
                -- Grab the constraints if there is any
                (constraints, sigWithoutConstraints) =
                  case sigWithoutUsage of
                    Sexp.List [constArrow, constraints, body]
                      | Sexp.isAtomNamed constArrow ":=>",
                        Just xs <- Sexp.toList constraints ->
                        (xs, body)
                    _ -> ([], sigWithoutUsage)
             in -- now our signature is without the extra information that
                -- belongs with other parameters in the ML signature
                Just $
                  Target.Sig
                    fName
                    (fmap expression usage)
                    (expression sigWithoutConstraints)
                    (fmap expression constraints)
     in Target.Func fName (fmap functionLike xs) actualSig
defunSig _ = error "malfromed defunSig"

----------------------------------------
-- Declarations
----------------------------------------

declaration :: Sexp.T -> Target.Declaration
declaration = Target.Infixivity . infixDeclar

infixDeclar :: Sexp.T -> Target.InfixDeclar
infixDeclar (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    if  | Sexp.isAtomNamed inf "infix" ->
          Target.NonAssoc atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixl" ->
          Target.AssocL atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixr" ->
          Target.AssocR atomName (fromInteger atomNum)
        | otherwise -> error "malformed declaration"
infixDeclar _ = error "malformed declaration"

----------------------------------------
-- Types
----------------------------------------

-- Top Level type transformations
type' :: Sexp.T -> Target.Type
type' (assocName Sexp.:> args Sexp.:> dat)
  | Just symbolList <- toSymbolList args =
    Target.Typ usage symName symbolList (adtF sig (adt dat))
  where
    Assoc {symName, usage, sig} = handleAssocTypeName assocName
    adtF = maybe Target.NonArrowed Target.Arrowed
type' _ = error "malformed type declaration"

adt :: Sexp.T -> Target.Adt
adt rec'@(Sexp.List [name Sexp.:> _])
  | recordDetector name =
    Target.Product (product rec')
adt sums =
  Target.Sum (sum sums)

sum :: Sexp.T -> NonEmpty Target.Sum
sum xs = Sexp.foldr f (pure (trans (Sexp.last xs))) (Sexp.butLast xs)
  where
    trans (name Sexp.:> contents)
      | Just n <- eleToSymbol name =
        case contents of
          Sexp.Nil ->
            Target.S n Nothing
          i ->
            Target.S n (Just (product i))
    trans _ = error "malformed sum"
    f ele =
      NonEmpty.cons (trans ele)

product :: Sexp.T -> Target.Product
product (Sexp.List [f@(name Sexp.:> form)])
  | recordDetector name =
    Target.Record (record f)
  | Sexp.isAtomNamed name ":arrow" =
    Target.Arrow (expression form)
product normal
  | Just list <- Sexp.toList normal =
    Target.ADTLike (expression <$> list)
product _ = error "malformed product"

record :: Sexp.T -> Target.Record
record (Sexp.List [name, form, sig])
  | Sexp.isAtomNamed name ":" =
    let Target.Record'' rec' _ = record form
     in Target.Record'' rec' (Just (expression sig))
record form' =
  Target.Record'' (NonEmpty.fromList (recorDHelp form)) Nothing
  where
    form = Sexp.cdr form'

recorDHelp :: Sexp.T -> [Target.NameType]
recorDHelp =
  maybe [] (fmap f) . Sexp.toList . Sexp.groupBy2
  where
    f (Sexp.List [n, sig]) = Target.NameType' (expression sig) (name n)
    f _ = error "malformed list"

name :: Sexp.T -> Target.Name
name (Sexp.List [n, a])
  | Sexp.isAtomNamed n ":implicit",
    Just atom <- eleToSymbol a =
    Target.Implicit atom
name a
  | Just atom <- eleToSymbol a = Target.Concrete atom
name _ = error "malformed record-name-field"

------------------------------
-- Type' Helper
------------------------------
recordDetector :: Sexp.T -> Bool
recordDetector name =
  Sexp.isAtomNamed name ":record-d" || Sexp.isAtomNamed name ":"

data AssocTypeName
  = Assoc
      { symName :: Symbol,
        usage :: Maybe Target.Expression,
        sig :: Maybe Target.Expression
      }
  deriving (Show, Eq)

-- | @handleAssocTypeName@ takes a type declaration name and gives us
-- back a direct record of it's contents transformed into the target
-- syntax
handleAssocTypeName :: Sexp.T -> AssocTypeName
handleAssocTypeName (name Sexp.:> properties)
  | Just atomName <- eleToSymbol name =
    Assoc
      { symName = atomName,
        usage = fmap expression (Sexp.assoc (Sexp.atom ":usage") group),
        sig = fmap expression (Sexp.assoc (Sexp.atom ":type") group)
      }
  where
    group = Sexp.groupBy2 properties
handleAssocTypeName name
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Assoc {symName = NameSym.toSymbol atomName, usage = Nothing, sig = Nothing}
  | otherwise = error "malformed type name"

--------------------------------------------------------------------------------
-- Misc Transformations
--------------------------------------------------------------------------------
----------------------------------------
-- Matching
----------------------------------------

-----------------------------------
-- argument specific bits
-----------------------------------

arg :: Sexp.T -> Target.Arg
arg (Sexp.List [i, t])
  | Sexp.isAtomNamed i ":implicit-a" =
    Target.ImplicitA (matchLogic t)
arg t =
  Target.ConcreteA (matchLogic t)

-----------------------------------
-- specific to match expressions
-----------------------------------
match :: Sexp.T -> Target.Match
match (t Sexp.:> binds)
  | Just xs <- Sexp.toList binds =
    Target.Match'' (expression t) (NonEmpty.fromList (fmap matchL xs))
match _ = error "malfromed match expression"

matchL :: Sexp.T -> Target.MatchL
matchL (Sexp.List [pat, body]) =
  Target.MatchL (matchLogic pat) (expression body)
matchL _ = error "malformed matchL"

---------------------------------------
-- General to all matches
---------------------------------------
matchLogic :: Sexp.T -> Target.MatchLogic
matchLogic (Sexp.List [i, n, start])
  | Sexp.isAtomNamed i ":as",
    Just s <- eleToSymbol n =
    Target.MatchLogic (matchLogicStart start) (Just s)
matchLogic t =
  Target.MatchLogic (matchLogicStart t) Nothing

matchLogicStart :: Sexp.T -> Target.MatchLogicStart
matchLogicStart x
  | Just ele <- eleToSymbol x =
    Target.MatchName ele
  | Just Sexp.N {atomNum} <- Sexp.atomFromT x =
    Target.MatchConst (Target.Number (Target.Integer' atomNum))
matchLogicStart (recordOrCon Sexp.:> rest)
  -- gotta group by 2 in this case
  | Sexp.isAtomNamed recordOrCon ":record-no-pun" =
    Target.MatchRecord (NonEmpty.fromList (fmap (nameSet matchLogic) grouped))
  -- we should have a constructor name in this instance
  | Just Sexp.A {atomName} <- Sexp.atomFromT recordOrCon,
    Just xs <- Sexp.toList rest =
    Target.MatchCon atomName (fmap matchLogic xs)
  where
    Just grouped = Sexp.toList (Sexp.groupBy2 rest)
matchLogicStart _ = error "malformed matchLogicStart"

nameSet :: (Sexp.T -> t) -> Sexp.T -> Target.NameSet t
nameSet trans (Sexp.List [field, assignedName])
  | Just Sexp.A {atomName} <- Sexp.atomFromT field =
    Target.NonPunned atomName (trans assignedName)
nameSet _ _ = error "bad nameSet"

--------------------------------------------------------------------------------
-- Expression Transformation
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Function Like
------------------------------------------------------------
functionLike :: Sexp.T -> Target.FunctionLike Target.Expression
functionLike (Sexp.List [args, body])
  | Just xs <- Sexp.toList args =
    Target.Like (fmap arg xs) (expression body)
functionLike _ = error "malformed like"

------------------------------------------------------------
-- Binders
------------------------------------------------------------

lambda :: Sexp.T -> Target.Lambda
lambda (Sexp.List [args, body])
  | Just xs <- Sexp.toList args >>= NonEmpty.nonEmpty =
    Target.Lamb (fmap matchLogic xs) (expression body)
lambda _ = error "malformed lambda"

let' :: Sexp.T -> Target.Let
let' (Sexp.List [name, fun, body])
  | Just name <- eleToSymbol name,
    Just xs <- Sexp.toList grouped >>= NonEmpty.nonEmpty =
    Target.LetGroup
      name
      (fmap functionLike xs)
      (expression body)
  where
    grouped = Sexp.groupBy2 fun
let' _ = error "malformed let"

letType :: Sexp.T -> Target.LetType
letType (Sexp.List [name, args, dat, body]) =
  Target.LetType'' (type' (Sexp.listStar [name, args, dat])) (expression body)
letType _ = error "malformed let-type"

------------------------------------------------------------
-- Others
------------------------------------------------------------

universeExpression :: Sexp.T -> Target.UniverseExpression
universeExpression (Sexp.List [a])
  | Just name <- eleToSymbol a = Target.UniverseExpression name
universeExpression _ = error "malformed universe expression"

declarationExpression :: Sexp.T -> Target.DeclarationExpression
declarationExpression (Sexp.List [d, e]) =
  Target.DeclareExpression (declaration d) (expression e)
declarationExpression _ = error "malformed declaration"

infix' :: Sexp.T -> Target.Infix
infix' (Sexp.List [op, left, right])
  | Just Sexp.A {atomName} <- Sexp.atomFromT op =
    Target.Inf (expression left) atomName (expression right)
infix' _ = error "malformed infix"

primitive :: Sexp.T -> Target.Primitive
primitive (Sexp.List [p])
  | Just Sexp.A {atomName} <- Sexp.atomFromT p = Target.Prim atomName
primitive _ = error "improper primitive"

expRecord :: Sexp.T -> Target.ExpRecord
expRecord rest =
  Target.ExpressionRecord (NonEmpty.fromList (fmap (nameSet expression) grouped))
  where
    Just grouped = Sexp.toList (Sexp.groupBy2 rest)

typeRefine :: Sexp.T -> Target.TypeRefine
typeRefine (Sexp.List [name, refine]) =
  Target.TypeRefine (expression name) (expression refine)
typeRefine _ = error "malformed type refinement"

openExpr :: Sexp.T -> Target.ModuleOpenExpr
openExpr (Sexp.List [mod, expr])
  | Just Sexp.A {atomName} <- Sexp.atomFromT mod =
    Target.OpenExpress atomName (expression expr)
openExpr _ = error "malformed open"

parened :: Sexp.T -> Target.Expression
parened (Sexp.List [body]) = expression body
parened _ = error "malformed parened"

arrowExp :: Sexp.T -> Target.ArrowExp
arrowExp (Sexp.List [u, l, r]) = Target.Arr' (expression l) (expression u) (expression r)
arrowExp _ = error "malformed arrow expression"

tuple :: Sexp.T -> Target.Tuple
tuple t
  | Just xs <- Sexp.toList t = Target.TupleLit (fmap expression xs)
tuple _ = error "malformed tuple"

list' :: Sexp.T -> Target.List
list' t
  | Just xs <- Sexp.toList t = Target.ListLit (fmap expression xs)
list' _ = error "malformed list"

block :: Sexp.T -> Target.Block
block (Sexp.List [b]) = Target.Bloc (expression b)
block _ = error "malformed block"

application :: Sexp.T -> Target.Application
application (name Sexp.:> args)
  | Just xs <- Sexp.toList args >>= NonEmpty.nonEmpty =
    Target.App (expression name) (fmap expression xs)
application _ = error "malformed application"

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSym.toSymbol atomName)
  | otherwise = Nothing

toSymbolList :: Sexp.T -> Maybe [Symbol]
toSymbolList x = Sexp.toList x >>= traverse eleToSymbol
