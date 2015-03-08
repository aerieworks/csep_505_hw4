module TestCases where

import Expr
import Result
import TypeCheck

problem1Tests = [
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

runProblem1Test (t1, t2, expected) =
  case ((t1 == t2) == expected) of
    True -> Ok ""
    False -> Err ("Expected " ++ (show expected) ++ ": " ++ (show t1) ++ " == " ++ (show t2))

runProblem1Tests =
  map runProblem1Test problem1Tests

problem2Tests = [
  ((ArrowT (VarT "a") BoolT), ["a"]),
  ((PairT (PairT (VarT "a") (VarT "b")) (PairT (VarT "c") (VarT "d"))), ["a", "b", "c", "d"]),
  ((ListT (VarT "a")), ["a"]),
  ((ListT StringT), []),
  ((ForAllT "a" (PairT (VarT "b") (VarT "a"))), ["b"]),
  ((ForAllT "a" (PairT NumT (VarT "a"))), []),
  ((ForAllT "a" (PairT (ForAllT "b" (ArrowT (VarT "a") (VarT "a"))) (VarT "b"))), ["b"]),
  ((PairT StringT (ForAllT "a" (PairT (VarT "a") (VarT "b")))), ["b"])
  ]

runProblem2Test (t, expected) =
  case (freeTypeVars t []) == expected of
    True -> Ok ""
    False -> Err ("Expected " ++ (show expected) ++ ": " ++ (show t))

runProblem2Tests =
  map runProblem2Test problem2Tests

