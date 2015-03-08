module TestCases where

import Expr
import Result

testCases = [
  (VarT "c", VarT "c", True),
  (VarT "d", VarT "e", False),
  ((ForAllT "x" (VarT "x")), (ForAllT "y" (VarT "y")), True),
  ((ForAllT "x" (PairT (VarT "x") (VarT "y"))),
    (ForAllT "y" (PairT (VarT "y") (VarT "x"))),
    False),
  ((ForAllT "a" (ArrowT (VarT "a") (VarT "c"))),
    (ForAllT "b" (ArrowT (VarT "b") (VarT "c"))),
    True),
  ((ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b") (PairT (VarT "a") (VarT "b")))))),
    (ForAllT "b" (ForAllT "c" (ArrowT (VarT "b") (ArrowT (VarT "c") (PairT (VarT "b") (VarT "c")))))),
    True),
  ((ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b") (PairT (VarT "a") (VarT "b")))))),
    (ForAllT "b" (ForAllT "c" (ArrowT (VarT "c") (ArrowT (VarT "b") (PairT (VarT "b") (VarT "c")))))),
    False),
  ((ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b") (PairT (VarT "a") (VarT "b")))))),
    (ForAllT "b" (ForAllT "a" (ArrowT (VarT "a") (ArrowT (VarT "b") (PairT (VarT "a") (VarT "b")))))),
    False),
  ((PairT (VarT "a") NumT), (PairT BoolT (VarT "b")), False),
  ((ForAllT "a" (ForAllT "b" (PairT (VarT "a") (VarT "b")))),
    (ForAllT "a" (ForAllT "a" (PairT (VarT "a") (VarT "a")))),
    False)
  ]

runTest (t1, t2, expected) =
  case ((t1 == t2) == expected) of
    True -> Ok ""
    False -> Err ("Expected " ++ (show expected) ++ ": " ++ (show t1) ++ " == " ++ (show t2))

runAllTests =
  map runTest testCases