-- | Passes contains a list of passes on the frontend syntax that can be
-- done with no extra information needed. Thus we export the following passes
--
-- - Removing Explicit Module declarations
-- - Removing Guards
-- - Conds ⟶ If ⟶ Match
-- - Combining signatures to functions
-- - Removing punned record arguments
-- - Remvoing Do syntax
module Juvix.Desugar.Passes
  ( moduleTransform,
    condTransform,
    ifTransform,
    multipleTransDefun,
    combineSig,
    multipleTransLet,
    translateDo,
    removePunnedRecords,
    moduleLetTransform,
  )
where

import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

--------------------------------------------------------------------------------
-- Fully Translated
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Cond Desugar Passes
------------------------------------------------------------

-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:Cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== ":cond") condToIf
  where
    condToIf atom cdr =
      let acc =
            generation (Sexp.last cdr) Sexp.Nil
              |> Sexp.butLast
       in Sexp.foldr generation acc (Sexp.butLast cdr)
            |> Sexp.addMetaToCar atom
    --
    generation (Sexp.Cons condition body) acc =
      Sexp.list [Sexp.atom "if", condition, Sexp.car body, acc]
    generation _ _ =
      error "malformed cond"

-- | @ifTransform@ - transforms a generated if form into a case
-- - BNF input form:
--   1. (if pred then else)
--   2. (if pred then)
-- - BNF output form:
--   1. (case pred (true then) (false else))
--   2. (case pred (true then))
-- - Note =case=, =then=, and =else= are literals
ifTransform :: Sexp.T -> Sexp.T
ifTransform xs = Sexp.foldPred xs (== "if") ifToCase
  where
    ifToCase atom cdr =
      case cdr of
        Sexp.List [pred, then', else'] ->
          Sexp.list (caseListElse pred then' else')
        Sexp.List [pred, then'] ->
          Sexp.list (caseList pred then')
        _ ->
          error "malformed if"
            |> Sexp.addMetaToCar atom
    caseList pred then' =
      [Sexp.atom "case", pred, Sexp.list [Sexp.list [Sexp.atom "True"], then']]
    caseListElse pred then' else' =
      caseList pred then' <> [Sexp.list [Sexp.list [Sexp.atom "False"], else']]

------------------------------------------------------------
-- Defun Transformation
------------------------------------------------------------

-- These transform function like things,
-- TODO ∷ re-use code more between the first 2 passes here
--

-- | @multipleTransLet@ - transforms multiple let forms
-- into a single let-match
--
-- - BNF input form:
--   + (let f (arg-match-11 … arg-match-1n)     body-1
--       (let f (arg-match-21 … arg-match-2n)   body-2
--         …
--         (let f (arg-match-n1 … arg-match-nn) body-n
--            rest)))
-- - BNF output form
--   + (:let-match f ((args-match-11 … args-match-1n) body-1
--                    (args-match-21 … args-match-2n) body-2
--                    …
--                    (args-match-n1 … args-match-nn) body-n)
--        rest)
-- - Note the f's are exactly the same name
multipleTransLet :: Sexp.T -> Sexp.T
multipleTransLet xs = Sexp.foldPred xs (== "let") letToLetMatch
  where
    letToLetMatch atom (Sexp.List [a@(Sexp.Atom (Sexp.A name _)), bindings, body, rest]) =
      let (grabbed, notMatched) = grabSimilar name rest
       in Sexp.list
            [ Sexp.atom ":let-match",
              a,
              putTogetherSplices (Sexp.list [bindings, body] : grabbed),
              notMatched
            ]
            |> Sexp.addMetaToCar atom
    letToLetMatch _atom _ =
      error "malformed let"
    --
    grabSimilar name (Sexp.List [let1, name1, bindings, body, rest])
      | Sexp.isAtomNamed let1 "let" && Sexp.isAtomNamed name1 name =
        grabSimilar name rest
          |> first (Sexp.list [bindings, body] :)
    grabSimilar _name xs = ([], xs)
    --
    putTogetherSplices =
      foldr spliceBindingBody Sexp.Nil
    --
    spliceBindingBody (Sexp.List [bindings, body]) acc =
      Sexp.Cons bindings (Sexp.Cons body acc)
    spliceBindingBody _ _ =
      error "doesn't happen"

-- This one and sig combining are odd mans out, as they happen on a
-- list of transforms
-- We will get rid of this as this should be the job of Code -> Context!

-- | @multipleTransDefun@ - trasnforms multiple defun forms into a
-- single defun match form
-- - Input BNF:
--   + (:defun f (arg-11 … arg-1n) body-1)
--     (:defun f (arg-21 … arg-2n) body-2)
--     …
--     (:defun f (arg-n1 … arg-nn) body-n)
-- - Output BNF:
--   + (:defun-match f
--       ((arg-11 … arg-1n) body-1)
--       ((arg-21 … arg-2n) body-2)
--       …
--       ((arg-n1 … arg-nn) body-n))
-- - Notes :: We could replace the out layer of ()'s with nothing to
--   reduce the numbers of ()'s
multipleTransDefun :: [Sexp.T] -> [Sexp.T]
multipleTransDefun = search
  where
    search (defun@(Sexp.List (defun1 : name1@(Sexp.Atom a) : _)) : xs)
      | Sexp.isAtomNamed defun1 ":defun",
        Just name <- Sexp.nameFromT name1 =
        let (sameDefun, toSearch) = grabSimilar name xs
         in combineMultiple name (defun : sameDefun)
              |> Sexp.addMetaToCar a
              |> (: search toSearch)
    search (x : xs) = x : search xs
    search [] = []
    combineMultiple name xs =
      Sexp.list ([Sexp.atom ":defun-match", Sexp.atom name] <> (Sexp.cdr . Sexp.cdr <$> xs))
    sameName name (Sexp.List (defun1 : name1 : _))
      | Sexp.isAtomNamed defun1 ":defun" && Sexp.isAtomNamed name1 name =
        True
    sameName _ _ =
      False
    grabSimilar _nam [] = ([], [])
    grabSimilar name (defn : xs)
      | sameName name defn =
        let (same, rest) = grabSimilar name xs
         in (defn : same, rest)
      | otherwise =
        ([], defn : xs)

-- This pass will also be removed, but is here for comparability
-- reasons we just drop sigs with no defuns for now ☹. Fix this up when
-- we remove this pass

-- | @combineSig@ - combines a sig and a defun-match to form a sig-match
-- - Input BNF:
--   1. (:defsig f signature)
--      (:defun-match f
--        ((arg-n1 … arg-nn) body-1)
--        …
--        ((arg-n1 … arg-nn) body-n))
--   2. (:defun-match f
--        ((arg-n1 … arg-nn) body-1)
--        …
--        ((arg-n1 … arg-nn) body-n))
-- - Output BNF:
--   1. (:defsig-match f signature
--        ((arg-11 … arg-1n) body-1)
--         …
--        ((arg-n1 … arg-nn) body-n))
--   2. (:defsig-match f ()
--         ((arg-n1 … arg-nn) body-1)
--         …
--         ((arg-n1 … arg-nn) body-n))
combineSig :: [Sexp.T] -> [Sexp.T]
combineSig
  ( (Sexp.Atom (Sexp.A ":defsig" _) Sexp.:> name Sexp.:> sig Sexp.:> Sexp.Nil) :
      (Sexp.Atom a@(Sexp.A ":defun-match" _) Sexp.:> defName Sexp.:> body) :
      xs
    )
    | defName == name =
      Sexp.addMetaToCar a (Sexp.listStar [Sexp.atom ":defsig-match", name, sig, body])
        : combineSig xs
combineSig ((Sexp.Atom a@(Sexp.A ":defun-match" _) Sexp.:> defName Sexp.:> body) : xs) =
  Sexp.addMetaToCar a (Sexp.listStar [Sexp.atom ":defsig-match", defName, Sexp.Nil, body])
    : combineSig xs
combineSig (Sexp.List [Sexp.Atom (Sexp.A ":defsig" _), _, _] : xs) =
  combineSig xs
combineSig (x : xs) = x : combineSig xs
combineSig [] = []

------------------------------------------------------------
-- Misc transformations
------------------------------------------------------------

-- | @translateDo@ - removes the do syntax from the frontend syntax
-- - Input BNF:
--   + (:do
--       (%<- name-1 body-1)
--       body-2
--       body-3
--       …
--       (%<- name-n body-n)
--       return)
-- - Output BNF:
--   + (Prelude.>>= body-1
--        (lambda (name-1)
--          (Prelude.>> body-2
--             (Prelude.>> body-n
--                (… (Prelude.>>= body-n (lambda (name-n) return)))))))
translateDo :: Sexp.T -> Sexp.T
translateDo xs = Sexp.foldPred xs (== ":do") doToBind
  where
    doToBind atom sexp =
      Sexp.foldr generation acc (Sexp.butLast sexp)
        |> Sexp.addMetaToCar atom
      where
        acc =
          case Sexp.last sexp of
            -- toss away last %<-... we should likely throw a warning for this
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), _name, body] -> body
            xs -> xs
        generation body acc =
          case body of
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), name, body] ->
              Sexp.list
                [ Sexp.atom "Prelude.>>=",
                  body,
                  Sexp.list [Sexp.atom "lambda", Sexp.list [name], acc]
                ]
            notBinding ->
              Sexp.list [Sexp.atom "Prelude.>>", notBinding, acc]

-- | @removePunnedRecords@ - removes the record puns from the syntax to
-- have an uniform a-list
-- - BNF input:
--   + (:record (punned-1) (name-2 body-2) … (punned-n))
-- - BNF output:
--   + (:record punned-1 punned-1 name-2 body-2 … punned-n punned-n)
removePunnedRecords :: Sexp.T -> Sexp.T
removePunnedRecords xs = Sexp.foldPred xs (== ":record") removePunned
  where
    removePunned atom sexp =
      Sexp.listStar
        [ Sexp.atom ":record-no-pun",
          Sexp.foldr f Sexp.Nil sexp
        ]
        |> Sexp.addMetaToCar atom
      where
        f (Sexp.List [field, bind]) acc =
          field Sexp.:> bind Sexp.:> acc
        f (Sexp.List [pun]) acc =
          pun Sexp.:> pun Sexp.:> acc
        f _ _ = error "malformed record"

------------------------------------------------------------
-- Module Pass
------------------------------------------------------------

-- Update this two fold.
-- 1. remove the inner combine and make it global
-- 2. Find a way to handle the cond case

-- | @moduleTransform@ - transforms a module and top level statements
-- into expressions
-- - _Top Level Transformation_
--   + defun     ⟶ let
--   + type      ⟶ let-type
--   + defsig    ⟶ let-sig
--   + declare   ⟶ declaim
--   + open      ⟶ open-in
--   + defmodule ⟶ let-mod
-- - BNF Input Form
--   1. (:defmodule name (arg1 … argn) toplevel-1 … toplevel-n)
--   2. (:defmodule name (arg1 … argn)
--         (cond
--           (pred-1 toplevel-11 … toplevel-1n)
--           …
--           (pred-n toplevel-n1 … toplevel-nn)))
-- - BNF output form
--   1. (defun name (arg1 … argn)
--        (expression-1 (… expression-n …
--                        (record (toplevel-1-name) … (toplevel-n-name)))))
--   2. (defun name (arg1 … argn)
--        (cond
--           (pred-1 (expression-11
--                      (… expression-1n …
--                         (record (toplevel-11-name) … (toplevel-1n-name)))))
--           …
--           (pred-n (expression-n1
--                      (… expression-nn …
--                         (record (toplevel-n1-name) … (toplevel-nn-name)))))))
-- - Where Expression follows the Top level Transformation, and
--   <foo-name> is the name of foo
moduleTransform :: Sexp.T -> Sexp.T
moduleTransform xs = Sexp.foldPred xs (== ":defmodule") moduleToRecord
  where
    moduleToRecord atom (name Sexp.:> args Sexp.:> body) =
      Sexp.list
        [ Sexp.atom ":defun",
          name,
          args,
          ignoreCond body (\b -> Sexp.foldr combine (generatedRecord b) b)
        ]
        |> Sexp.addMetaToCar atom
    moduleToRecord _ _ = error "malformed record"

-- | @moduleLetTransform@ - See @moduleTransform@'s comment
moduleLetTransform :: Sexp.T -> Sexp.T
moduleLetTransform xs = Sexp.foldPred xs (== ":let-mod") moduleToRecord
  where
    moduleToRecord atom (name Sexp.:> args Sexp.:> body Sexp.:> rest) =
      Sexp.list
        [ Sexp.atom "let",
          name,
          args,
          ignoreCond body (\b -> Sexp.foldr combine (generatedRecord b) b),
          rest
        ]
        |> Sexp.addMetaToCar atom
    moduleToRecord _ _ = error "malformed record"

----------------------------------------
-- Module Helpers
----------------------------------------

-- | @combine@ - is the helper for transforming top level statements
-- into expressions
combine :: Sexp.T -> Sexp.T -> Sexp.T
combine (form Sexp.:> name Sexp.:> args Sexp.:> body Sexp.:> Sexp.Nil) expression
  | Sexp.isAtomNamed form ":defun" =
    -- we crunch the xs in a list
    Sexp.list [Sexp.atom "let", name, args, body, expression]
combine (form Sexp.:> name Sexp.:> xs) expression
  | Sexp.isAtomNamed form "type" =
    Sexp.list [Sexp.atom ":let-type", name, xs, expression]
combine (form Sexp.:> name Sexp.:> xs Sexp.:> Sexp.Nil) expression
  | Sexp.isAtomNamed form ":defsig" =
    Sexp.list [Sexp.atom ":let-sig", name, xs, expression]
combine (form Sexp.:> declaration) expression
  | Sexp.isAtomNamed form "declare" =
    Sexp.list [Sexp.atom ":declaim", declaration, expression]
combine (Sexp.List [form, open]) expression
  | Sexp.isAtomNamed form "open" =
    Sexp.list [Sexp.atom ":open-in", open, expression]
combine (form Sexp.:> name Sexp.:> args Sexp.:> xs) expression
  | Sexp.isAtomNamed form ":defmodule" =
    -- Turn this into a let-module
    if  | Sexp.isAtomNamed (Sexp.car xs) ":cond" ->
          Sexp.list [Sexp.atom ":let-mod", name, args, Sexp.car xs, expression]
        | otherwise ->
          Sexp.list [Sexp.atom ":let-mod", name, args, xs, expression]
-- ignore other forms
combine _ expression = expression

-- | @ignoreCond@ gets past the annoying cond cells for modules
ignoreCond :: Sexp.T -> (Sexp.T -> Sexp.T) -> Sexp.T
ignoreCond ((form Sexp.:> xs) Sexp.:> Sexp.Nil) trans
  | Sexp.isAtomNamed form ":cond" =
    Sexp.listStar [form, Sexp.foldr comb Sexp.Nil xs]
  where
    comb (pred Sexp.:> body) acc =
      Sexp.listStar [Sexp.list [pred, trans body], acc]
    comb _ acc = acc
ignoreCond xs trans = trans xs

-- | @generatedRecord@ - record generation
generatedRecord :: Sexp.T -> Sexp.T
generatedRecord b =
  Sexp.list
    (Sexp.atom ":record" : fmap (\x -> Sexp.list [Sexp.Atom x]) (names b))

-- | @names@ - folding @grabNames@ that uniquifyies the result to
-- achieve an unique list
names :: Sexp.T -> [Sexp.Atom]
names body = Sexp.foldr grabNames [] body |> Set.fromList |> Set.toList

-- | @grabNames@ - responsible for grabbing the names out of top levels
grabNames :: Sexp.T -> [Sexp.Atom] -> [Sexp.Atom]
grabNames (form Sexp.:> name Sexp.:> _) acc
  | Sexp.isAtomNamed form ":defun"
      || Sexp.isAtomNamed form "type"
      || Sexp.isAtomNamed form ":defmodule"
      || Sexp.isAtomNamed form ":defsig",
    Just name <- Sexp.atomFromT name =
    name : acc
  | Sexp.isAtomNamed (Sexp.car name) "type",
    Just name <- Sexp.atomFromT (Sexp.car name) =
    name : acc
grabNames _ acc = acc
