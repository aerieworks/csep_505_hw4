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
    True -> Ok ()
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
    True -> Ok ()
    False -> Err ("Expected " ++ (show expected) ++ ": " ++ (show t))

runProblem2Tests =
  map runProblem2Test problem2Tests


problem3Tests = [
  ("a", "b", (ListT (VarT "a")), (ListT (VarT "b"))),
  ("a", "b", (ForAllT "a" (PairT (VarT "a") BoolT)), (ForAllT "b" (PairT (VarT "b") BoolT))),
  ("a", "b", (PairT (VarT "a") (VarT "c")), (PairT (VarT "b") (VarT "c")))
  ]

runProblem3Test (vIn, vOut, t, expected) =
  case (alphaRename vIn vOut t) == expected of
    True -> Ok ()
    False -> Err ("Expected " ++ (show expected) ++ ": " ++ (show vIn) ++ " -> " ++ (show vOut) ++ ", " ++ (show t))

runProblem3Tests =
  map runProblem3Test problem3Tests


problem4Tests = [
  ("a", NumT, (ArrowT (VarT "a") (VarT "a")), (ArrowT NumT NumT)),
  ("b", (PairT NumT BoolT), (ListT (VarT "b")), (ListT (PairT NumT BoolT))),
  ("a", NumT, (PairT (VarT "a") (ForAllT "a" (ArrowT (VarT "a") (VarT "a")))), (PairT NumT (ForAllT "a" (ArrowT (VarT "a") (VarT "a"))))),
  ("a", (PairT BoolT (VarT "b")), (ForAllT "b" (ArrowT (VarT "a") (VarT "b"))), (ForAllT "c" (ArrowT (PairT BoolT (VarT "b")) (VarT "c")))),
  ("a", NumT, (ForAllT "c" (VarT "f")), (ForAllT "c" (VarT "f"))),
  ("a", BoolT, (ForAllT "a" (VarT "a")), (ForAllT "a" (VarT "a"))),
  ("a", BoolT, (ForAllT "a" (VarT "b")), (ForAllT "a" (VarT "b"))),
  ("a", BoolT, (ForAllT "b" (VarT "a")), (ForAllT "b" BoolT)),
  ("a", BoolT, (ForAllT "b" (VarT "b")), (ForAllT "b" (VarT "b"))),
  ("b", BoolT, (ForAllT "a" (VarT "a")), (ForAllT "a" (VarT "a"))),
  ("b", BoolT, (ForAllT "a" (VarT "b")), (ForAllT "a" BoolT)),
  ("b", BoolT, (ForAllT "b" (VarT "a")), (ForAllT "b" (VarT "a"))),
  ("b", BoolT, (ForAllT "b" (VarT "b")), (ForAllT "b" (VarT "b"))),
  ("a",
    (ForAllT "b" (ArrowT (VarT "c") (VarT "b"))),
    (ArrowT (ForAllT "c" (ArrowT (VarT "c") (VarT "a"))) (VarT "b")),
    (ArrowT (ForAllT "d" (ArrowT (VarT "d") (ForAllT "b" (ArrowT (VarT "c") (VarT "b"))))) (VarT "b")))
  ]

runProblem4Test (v, forType, inType, expected) =
  let actual = (subst v forType inType)
  in case actual == expected of
    True -> Ok ()
    False -> Err ("Expected: " ++ (show expected) ++ "; Actual: " ++ (show actual))

runProblem4Tests =
  map runProblem4Test problem4Tests


problem5Tests = [
  ("5", Ok (NumT)),
  ("\"abc\"", Ok (StringT)),
  ("x", Err "Untyped variable `\"x\"`"),
  ("true", Ok (BoolT)),
  ("false", Ok (BoolT)),
  ("+", Ok (ArrowT NumT (ArrowT NumT NumT))),
  ("(+ 5)", Ok (ArrowT NumT NumT)),
  ("(+ 5 3)", Ok (NumT)),
  ("(+ false)", Err "Function requires num but was called with bool"),
  ("(+ 5 false)", Err "Function requires num but was called with bool"),
  ("=", Ok (ArrowT NumT (ArrowT NumT BoolT))),
  ("(= 5)", Ok (ArrowT NumT BoolT)),
  ("(= 5 3)", Ok (BoolT)),
  ("(= false)", Err "Function requires num but was called with bool"),
  ("(= 5 false)", Err "Function requires num but was called with bool"),
  ("(= + *)", Err "Function requires num but was called with (num -> (num -> num))"),
  ("(if (= 5 3) 1 2)", Ok (NumT)),
  ("(if (= 5 3) false true)", Ok (BoolT)),
  ("(if (+ 5 3) false true)", Err "If expression expects boolean condition, but found num"),
  ("(if = false true)", Err "If expression expects boolean condition, but found (num -> (num -> bool))"),
  ("(if (= 5 3) 5 true)",
    Err "Type mismatch between if expression condition and alternate: num != bool"),
  ("(fun ([x : num]) (* x x))", Ok (ArrowT NumT NumT)),
  ("(fun ([x : num]) (+ x))", Ok (ArrowT NumT (ArrowT NumT NumT))),
  ("(fun ([x : bool]) (if x false true))", Ok (ArrowT BoolT BoolT)),
  ("(fun ([x : num] [y : num]) (if (< x y) x y))", Ok (ArrowT NumT (ArrowT NumT NumT))),
  ("(fun ([x : num] [y : bool]) (if (< x y) x y))", Err "Function requires num but was called with bool"),
  ("(fun ([x : bool]) (if x + *))", Ok (ArrowT BoolT (ArrowT NumT (ArrowT NumT NumT)))),
  ("(fun ([op : (num -> (num -> num))] [x : num] [y : num]) (op x y))",
    Ok (ArrowT (ArrowT NumT (ArrowT NumT NumT)) (ArrowT NumT (ArrowT NumT NumT))))
  --("(forall (a) (fun ([x : a] [l : (list num)]) (cons x l)))", Ok (ForAllT "a" (ArrowT (VarT "a") (ArrowT (ListT NumT) (ListT (VarT "a"))))))
  ]

runProblem5Test (inputStr, expected) =
  let actual = (parseAndCheckStr inputStr)
  in case actual == expected of
    True -> Ok ()
    False -> Err ("Expected: " ++ (show expected) ++ "; Actual: " ++ (show actual) ++
      " --- In: " ++ (show inputStr))

runProblem5Tests =
  map runProblem5Test problem5Tests
runAllTests =
  [ runProblem1Tests,
    runProblem2Tests,
    runProblem3Tests,
    runProblem4Tests,
    runProblem5Tests
    ]
