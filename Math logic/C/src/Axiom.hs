module Axiom where

import           Expression

-- A -> B -> A
isAxiom'1' :: Expression -> Bool
isAxiom'1' (a'1 :->: _ :->: a'2) = a'1 == a'2
isAxiom'1' _                     = False

-- (A -> B) -> (A -> B -> C) -> (A -> C)
isAxiom'2' :: Expression -> Bool
isAxiom'2' ((a'1 :->: b'1) :->: (a'2 :->: b'2 :->: c'1) :->: (a'3 :->: c'2)) =
  and [a'1 == a'2, a'1 == a'3, b'1 == b'2, c'1 == c'2]
isAxiom'2' _ = False

-- A -> B -> (A & B)
isAxiom'3' :: Expression -> Bool
isAxiom'3' (a'1 :->: b'1 :->: (a'2 :&: b'2)) = (a'1 == a'2) && (b'1 == b'2)
isAxiom'3' _                                 = False

-- (A & B) -> A
isAxiom'4' :: Expression -> Bool
isAxiom'4' ((a'1 :&: _) :->: a'2) = a'1 == a'2
isAxiom'4' _                      = False

-- (A & B) -> B
isAxiom'5' :: Expression -> Bool
isAxiom'5' ((_ :&: b'1) :->: b'2) = b'1 == b'2
isAxiom'5' _                      = False

-- A -> (A | B)
isAxiom'6' :: Expression -> Bool
isAxiom'6' (a'1 :->: (a'2 :|: _)) = a'1 == a'2
isAxiom'6' _                      = False

-- B -> (A | B)
isAxiom'7' :: Expression -> Bool
isAxiom'7' (b'1 :->: (_ :|: b'2)) = b'1 == b'2
isAxiom'7' _                      = False

-- (A -> C) -> (B -> C) -> (A | B -> C)
isAxiom'8' :: Expression -> Bool
isAxiom'8' ((a'1 :->: c'1) :->: (b'1 :->: c'2) :->: (a'2 :|: b'2 :->: c'3)) =
  and [a'1 == a'2, b'1 == b'2, c'1 == c'2, c'1 == c'3]
isAxiom'8' _ = False

-- (A -> B) -> (A -> !B) -> !A
isAxiom'9' :: Expression -> Bool
isAxiom'9' ((a'1 :->: b'1) :->: (a'2 :->: Not b'2) :->: Not a'3) = (a'1 == a'2) && (a'1 == a'3) && (b'1 == b'2)
isAxiom'9' _ = False

-- !!A -> A
isAxiom'10' :: Expression -> Bool
isAxiom'10' (Not (Not a'1) :->: a'2) = a'1 == a'2
isAxiom'10' _                        = False

-- A -> !A -> B
isAxiom'11' :: Expression -> Bool
isAxiom'11' (a'1 :->: Not a'2 :->: _) = a'1 == a'2
isAxiom'11' _                         = False

isAnyAxiom :: Expression -> Bool
isAnyAxiom e =
  or $
  (\f -> f e) <$>
  [ isAxiom'1'
  , isAxiom'2'
  , isAxiom'3'
  , isAxiom'4'
  , isAxiom'5'
  , isAxiom'6'
  , isAxiom'7'
  , isAxiom'8'
  , isAxiom'9'
  , isAxiom'11'
  ]

whichAxiom :: Expression -> Maybe Int
whichAxiom expr
  | isAxiom'1' expr = Just 1
  | isAxiom'2' expr = Just 2
  | isAxiom'3' expr = Just 3
  | isAxiom'4' expr = Just 4
  | isAxiom'5' expr = Just 5
  | isAxiom'6' expr = Just 6
  | isAxiom'7' expr = Just 7
  | isAxiom'8' expr = Just 8
  | isAxiom'9' expr = Just 9
  | isAxiom'10' expr = Just 10
  | otherwise = Nothing
