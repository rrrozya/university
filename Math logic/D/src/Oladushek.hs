module Oladushek where

import           Expression
import           ParseStr

ps = parseStr

data Info
  = Axiom
  | MP Expression

type ABC = (Expression, Expression, Expression)

type ExpInf = (Expression, Info)

reindex :: ABC -> Expression -> Expression
reindex (a, b, c) = helper
  where
    mapExpr :: String -> Expression
    mapExpr s
      | s == "A" = a
      | s == "B" = b
      | s == "C" = c
    --
    helper :: Expression -> Expression
    helper (Var var)  = mapExpr var
    helper (Not n)    = Not (helper n)
    helper (l :&: r)  = helper l :&: helper r
    helper (l :|: r)  = helper l :|: helper r
    helper (l :->: r) = helper l :->: helper r

reindexAll :: ABC -> [Expression] -> [Expression]
reindexAll abc = (reindex abc <$>)

tmpAtoA :: [ExpInf]
tmpAtoA =
  [ (ps "A -> A -> A", Axiom)
  , (ps "A -> (A -> A) -> A", Axiom)
  , (ps "(A -> A -> A) -> (A -> (A -> A) -> A) -> (A -> A)", Axiom)
  , (ps "(A -> (A -> A) -> A) -> (A -> A)", MP $ ps "A -> A -> A")
  , (ps "A -> A", MP $ ps "A -> (A -> A) -> A")
  ]

tmpAxiom :: [ExpInf]
tmpAxiom = [(ps "A", Axiom), (ps "A -> B -> A", Axiom), (ps "B -> A", MP $ ps "A")]

tmpMP :: [ExpInf]
tmpMP =
  [ (ps "(A -> C) -> ((A -> (C -> B)) -> (A -> B))", Axiom)
  , (ps "(A -> (C -> B)) -> (A -> B)", MP $ ps "A -> C")
  , (ps "A -> B", MP $ ps "A -> C -> B")
  ]

reindexExpInf :: ABC -> ExpInf -> ExpInf
reindexExpInf abc (e, Axiom)   = (reindex abc e, Axiom)
reindexExpInf abc (e, MP from) = (reindex abc e, MP $ reindex abc from)

genAtoA :: Expression -> [ExpInf]
genAtoA e = reindexExpInf (e, undefined, undefined) <$> tmpAtoA

genAxiom :: Expression -> Expression -> [ExpInf]
genAxiom e h = reindexExpInf (e, h, undefined) <$> tmpAxiom

genMP :: Expression -> Expression -> Expression -> [ExpInf]
genMP h e f = reindexExpInf (h, e, f) <$> tmpMP

deduction :: Expression -> [ExpInf] -> [ExpInf]
deduction _ [] = []
deduction h (eInf:tail) = helper eInf ++ deduction h tail
  where
    helper :: ExpInf -> [ExpInf]
    helper (e, Axiom) =
      if e == h
        then genAtoA e
        else genAxiom e h
    helper (e, MP from) = genMP h e from

andTT x y =
  reindexExpInf (x, y, undefined) <$>
  [(ps "A -> B -> A&B", Axiom), (ps "B -> A&B", MP $ ps "A"), (ps "A&B", MP $ ps "B")]

andTF x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "((A&B) -> B) -> ((A&B) -> !B) -> !(A&B)", Axiom)
  , (ps "A&B -> B", Axiom)
  , (ps "!B -> (A&B) -> !B", Axiom)
  , (ps "(A&B) -> !B", MP $ ps "!B")
  , (ps "(A&B -> !B) -> !(A&B)", MP $ ps "(A&B -> B)")
  , (ps "!(A&B)", MP $ ps "A&B -> !B")
  ]

andFT x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "((A&B) -> A) -> ((A&B) -> !A) -> !(A&B)", Axiom)
  , (ps "A&B -> A", Axiom)
  , (ps "!A -> (A&B) -> !A", Axiom)
  , (ps "(A&B) -> !A", MP $ ps "!A")
  , (ps "(A&B -> !A) -> !(A&B)", MP $ ps "A&B -> A")
  , (ps "!(A&B)", MP $ ps "A&B -> !A")
  ]

orTT x y = reindexExpInf (x, y, undefined) <$> [(ps "A -> A|B", Axiom), (ps "A|B", MP $ ps "A")]

orFT x y = reindexExpInf (x, y, undefined) <$> [(ps "B -> A | B", Axiom), (ps "A|B", MP $ ps "B")]

orFF x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "(A|B -> A) -> (A|B -> !A) -> !(A|B)", Axiom)
  , (ps "(A -> A) -> (B -> A) -> (A|B -> A)", Axiom)
  , (ps "A -> (A -> A) -> A", Axiom)
  , (ps "A -> A -> A", Axiom)
  , (ps "(A -> A -> A) -> (A -> (A -> A) -> A) -> (A -> A)", Axiom)
  , (ps "(A -> (A -> A) -> A) -> (A -> A)", MP $ ps "A -> A -> A")
  , (ps "A -> A", MP $ ps "A -> (A -> A) -> A")
  ] ++
  deduction (ps "!B") (deduction (ps "B") contradiction) ++
  [ (ps "B -> A", MP $ ps "!B")
  , (ps "(B -> A) -> (A|B -> A)", MP $ ps "A -> A")
  , (ps "(A|B) -> A", MP $ ps "B -> A")
  , (ps "(A|B -> !A) -> !(A|B)", MP $ ps "A|B -> A")
  , (ps "!A -> A|B -> !A", Axiom)
  , (ps "A|B -> !A", MP $ ps "!A")
  , (ps "!(A | B)", MP $ ps "A|B -> !A")
  ]
  where
    contradiction :: [ExpInf]
    contradiction =
      [ (ps "B -> !A -> B", Axiom)
      , (ps "!B -> !A -> !B", Axiom)
      , (ps "!B", Axiom)
      , (ps "B", Axiom)
      , (ps "!A -> B", MP $ ps "B")
      , (ps "!A -> !B", MP $ ps "!B")
      , (ps "(!A -> B) -> (!A -> !B) -> !!A", Axiom)
      , (ps "(!A -> !B) -> !!A", MP $ ps "!A -> B")
      , (ps "!!A", MP $ ps "!A -> !B")
      , (ps "!!A -> A", Axiom)
      , (ps "A", MP $ ps "!!A")
      ]

notNot x =
  reindexExpInf (x, undefined, undefined) <$>
  [ (ps "(!A -> A) -> (!A -> !A) -> !!A", Axiom)
  , (ps "A -> !A -> A", Axiom)
  , (ps "!A -> A", MP $ ps "A")
  , (ps "(!A -> !A) -> !!A", MP $ ps "!A -> A")
  , (ps "!A -> !A -> !A", Axiom)
  , (ps "!A -> (!A -> !A) -> !A", Axiom)
  , (ps "(!A -> !A -> !A) -> (!A -> (!A -> !A) -> !A) -> (!A -> !A)", Axiom)
  , (ps "(!A -> (!A -> !A) -> !A) -> (!A -> !A)", MP $ ps "!A -> !A -> !A")
  , (ps "!A -> !A", MP $ ps "!A -> (!A -> !A) -> !A")
  , (ps "!!A", MP $ ps "!A -> !A")
  ]

implTT x y = reindexExpInf (x, y, undefined) <$> [(ps "B -> A -> B", Axiom), (ps "A -> B", MP $ ps "B")]

implFT x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "(A -> !A) -> (A -> !A -> B) -> (A -> B)", Axiom)
  , (ps "!A -> A -> !A", Axiom)
  , (ps "A -> !A", MP $ ps "!A")
  , (ps "(A -> !A -> B) -> (A -> B)", MP $ ps "A -> !A")
  ] ++
  deduction (ps "A") (deduction (ps "!A") contradiction) ++ [(ps "(A -> B)", MP $ ps "A -> !A -> B")]
  where
    contradiction :: [ExpInf]
    contradiction =
      [ (ps "A -> !B -> A", Axiom)
      , (ps "!A -> !B -> !A", Axiom)
      , (ps "!A", Axiom)
      , (ps "A", Axiom)
      , (ps "!B -> A", MP $ ps "A")
      , (ps "!B -> !A", MP $ ps "!A")
      , (ps "(!B -> A) -> (!B -> !A) -> !!B", Axiom)
      , (ps "(!B -> !A) -> !!B", MP $ ps "!B -> A")
      , (ps "!!B", MP $ ps "!B -> !A")
      , (ps "!!B -> B", Axiom)
      , (ps "B", MP $ ps "!!B")
      ]

implTF x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "((A -> B) -> B) -> ((A -> B) -> !B) -> !(A -> B)", Axiom)
  , (ps "((A -> B) -> A) -> ((A -> B) -> (A -> B)) -> ((A -> B) -> B)", Axiom)
  , (ps "(A -> B) -> (A -> B) -> (A -> B)", Axiom)
  , (ps "(A -> B) -> ((A -> B) -> (A -> B)) -> (A -> B)", Axiom)
  , ( ps
        "((A -> B) -> (A -> B) -> (A -> B)) -> ((A -> B) -> ((A -> B) -> (A -> B)) -> (A -> B)) -> ((A -> B) -> (A -> B))"
    , Axiom)
  , ( ps "((A -> B) -> ((A -> B) -> (A -> B)) -> (A -> B)) -> ((A -> B) -> (A -> B))"
    , MP $ ps "(A -> B) -> (A -> B) -> (A -> B)")
  , (ps "((A -> B) -> (A -> B))", MP $ ps "(A -> B) -> ((A -> B) -> (A -> B)) -> (A -> B)")
  , (ps "!B -> (A -> B) -> !B", Axiom)
  , (ps "(A -> B) -> !B", MP $ ps "!B")
  , (ps "A -> (A -> B) -> A", Axiom)
  , (ps "(A -> B) -> A", MP $ ps "A")
  , (ps "((A -> B) -> (A -> B)) -> ((A -> B) -> B)", MP $ ps "(A -> B) -> A")
  , (ps "((A -> B) -> B)", MP $ ps "(A -> B) -> (A -> B)")
  , (ps "((A -> B) -> !B) -> !(A -> B)", MP $ ps "(A -> B) -> B")
  , (ps "!(A -> B)", MP $ ps "(A -> B) -> !B")
  ]

contr x y =
  reindexExpInf (x, y, undefined) <$>
  [ (ps "(A -> B) -> (A -> !B) -> !A", Axiom)
  , (ps "((A -> B) -> (A -> !B) -> !A) -> !B -> ((A -> B) -> (A -> !B) -> !A)", Axiom)
  , (ps "!B -> ((A -> B) -> (A -> !B) -> !A)", MP $ ps "(A -> B) -> (A -> !B) -> !A")
  , (ps "(A -> B) -> !B -> (A -> B)", Axiom)
  , (ps "A -> B", Axiom)
  , (ps "!B -> (A -> B)", MP $ ps "A -> B")
  , (ps "!B -> (A -> !B)", Axiom)
  , (ps "(!B -> (A -> B)) -> (!B -> ((A -> B) -> (A -> !B) -> !A)) -> (!B -> (A -> !B) -> !A)", Axiom)
  , (ps "((!B -> ((A -> B) -> (A -> !B) -> !A)) -> (!B -> (A -> !B) -> !A))", MP $ ps "!B -> (A -> B)")
  , (ps "!B -> (A -> !B) -> !A", MP $ ps "(!B -> ((A -> B) -> (A -> !B) -> !A))")
  , (ps "(!B -> A -> !B) -> (!B -> (A -> !B) -> !A) -> (!B -> !A)", Axiom)
  , (ps "(!B -> (A -> !B) -> !A) -> (!B -> !A)", MP $ ps "!B -> (A -> !B)")
  , (ps "!B -> !A", MP $ ps "!B -> (A -> !B) -> !A")
  ]

disjThird e =
  reindexExpInf (e, undefined, undefined) <$>
  [(ps "A -> A | !A", Axiom)] ++
  contr (ps "A") (ps "A | !A") ++
  [(ps "!A -> A | !A", Axiom)] ++
  contr (ps "!A") (ps "A | !A") ++
  [ (ps "(!(A|!A) -> !A) -> (!(A|!A) -> !!A) -> !!(A|!A)", Axiom)
  , (ps "(!(A|!A) -> !!A) -> !!(A|!A)", MP $ ps "!(A|!A) -> !A")
  , (ps "!!(A|!A)", MP $ ps "!(A|!A) -> !!A")
  , (ps "!!(A|!A) -> (A|!A)", Axiom)
  , (ps "A|!A", MP $ ps "!!(A|!A)")
  ]
