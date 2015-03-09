module TestCases where

import Expr
import Result
import TypeCheck
import InterpCont

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
    False),
  ((ForAllT "a" (ForAllT "b" (PairT (VarT "b") (VarT "b")))),
    (ForAllT "a" (ForAllT "a" (PairT (VarT "a") (VarT "a")))),
    True)
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
  ("(fun ([x : a]) x)", Err "unbound type var(s) in (a -> a): [\"a\",\"a\"]"),
  ("(fun ([x : num]) (* x x))", Ok (ArrowT NumT NumT)),
  ("(fun ([x : num]) (+ x))", Ok (ArrowT NumT (ArrowT NumT NumT))),
  ("(fun ([x : bool]) (if x false true))", Ok (ArrowT BoolT BoolT)),
  ("(fun ([x : num] [y : num]) (if (< x y) x y))", Ok (ArrowT NumT (ArrowT NumT NumT))),
  ("(fun ([x : num] [y : bool]) (if (< x y) x y))", Err "Function requires num but was called with bool"),
  ("(fun ([x : bool]) (if x + *))", Ok (ArrowT BoolT (ArrowT NumT (ArrowT NumT NumT)))),
  ("(fun ([op : (num -> (num -> num))] [x : num] [y : num]) (op x y))",
    Ok (ArrowT (ArrowT NumT (ArrowT NumT NumT)) (ArrowT NumT (ArrowT NumT NumT)))),
  ("(forall (a) (fun ([x : a]) ((cons <a>) x (empty <a>))))",
    Ok (ForAllT "a" (ArrowT (VarT "a") (ListT (VarT "a"))))),
  ("(forall (a) (fun ([x : a] [l : (list a)]) ((cons <a>) x l)))",
    Ok (ForAllT "a" (ArrowT (VarT "a") (ArrowT (ListT (VarT "a")) (ListT (VarT "a")))))),
  ("(with* ([conscons " ++
      "(forall (a) (fun ([x : a]) {(cons <a>) x ((cons <a>) x (empty <a>))}))]) " ++
        "((conscons <num>) 5))",
    Ok (ListT NumT)),
  ("(with* ([ mkpair (forall (a b) (fun ([x : a] [y : b]) { (pair <a b>) x y }))]) " ++
      "((mkpair <bool num>) true 5))",
    Ok (PairT BoolT NumT)),
  ("(forall (a b) (fun ([x : a] [y : b]) " ++
      "((cons <(a, b)>) ((pair <a b>) x y) (empty <(a, b)>))))",
    Ok (ForAllT "a" (ForAllT "b" (ArrowT (VarT "a") (ArrowT (VarT "b")
      (ListT (PairT (VarT "a") (VarT "b")))))))),

  -- Problem 5 tests from discussion board
  ("1", Ok (NumT)),
  ("(+ 2 4)", Ok (NumT)),
  ("(fun ([x : num]) (= x 5))", Ok (ArrowT NumT BoolT)),
  ("(with* ([id (forall (a) (fun ([y : a]) y))] [selfapp (fun ([x : (forall a.(a -> a))]) ((x <(forall b.(b -> b))>) x))]) (selfapp id))",
    Ok (ForAllT "b" (ArrowT (VarT "b") (VarT "b")))),
  ("(with* ([two (forall (a) (fun ([s : (a -> a)] [z : a]) (s (s z))))] " ++
           "[add (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                  "[m : ((a -> a) -> (a -> a))] " ++
                                  "[s : (a -> a)] " ++
                                  "[z : a]) ((n s) (m s z))))] " ++
           "[mult (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                   "[m : ((a -> a) -> (a -> a))] " ++
                                   "[s : (a -> a)]) (n (m s))))]) " ++
    "(((mult <num>) ((add <num>) (two <num>) (two <num>)) (two <num>)) (+ 1) 0))",
    Ok (NumT))
  ]

runProblem5Test (inputStr, expected) =
  let actual = (parseAndCheckStr inputStr)
  in case actual == expected of
    True -> Ok ()
    False -> Err ("Expected: " ++ (show expected) ++ "; Actual: " ++ (show actual) ++
      " --- In: " ++ (show inputStr))

runProblem5Tests =
  map runProblem5Test problem5Tests


problem6Tests = [
  ("5", NumC 5),
  ("\"abc\"", StringC "abc"),
  ("x", VarC "x"),
  ("true", VarC "true"),
  ("false", VarC "false"),
  ("+", VarC "+"),
  ("(+ 5)", AppC (VarC "+") (NumC 5)),
  ("(+ 5 3)", AppC (AppC (VarC "+") (NumC 5)) (NumC 3)),
  ("(+ false)", AppC (VarC "+") (VarC "false")),
  ("(+ 5 false)", AppC (AppC (VarC "+") (NumC 5)) (VarC "false")),
  ("(if (= 5 3) 1 2)", IfC (AppC (AppC (VarC "=") (NumC 5)) (NumC 3)) (NumC 1) (NumC 2)),
  ("(if (= 5 3) false true)",
    IfC (AppC (AppC (VarC "=") (NumC 5)) (NumC 3)) (VarC "false") (VarC "true")),
  ("(fun ([x : num]) (* x x))", FunC "x" (AppC (AppC (VarC "*") (VarC "x")) (VarC "x"))),
  ("(fun ([x : num]) (+ x))", FunC "x" (AppC (VarC "+") (VarC "x"))),
  ("(fun ([x : bool]) (if x false true))",
    FunC "x" (IfC (VarC "x") (VarC "false") (VarC "true"))),
  ("(fun ([x : num] [y : num]) (if (< x y) x y))",
    FunC "x" (FunC "y"
      (IfC (AppC (AppC (VarC "<") (VarC "x")) (VarC "y")) (VarC "x") (VarC "y")))),
  ("(forall (a) (fun ([x : a]) ((cons <a>) x (empty <a>))))",
    FunC "x" (AppC (AppC (VarC "cons") (VarC "x")) (VarC "empty"))),
  ("(forall (a) (fun ([x : a] [l : (list a)]) ((cons <a>) x l)))",
    FunC "x" (FunC "l"
      (AppC (AppC (VarC "cons") (VarC "x")) (VarC "l")))),
  ("(with* ([conscons " ++
      "(forall (a) (fun ([x : a]) {(cons <a>) x ((cons <a>) x (empty <a>))}))]) " ++
        "((conscons <num>) 5))",
    (AppC (FunC "conscons" (AppC (VarC "conscons") (NumC 5)))
      (FunC "x" (AppC
        (AppC (VarC "cons") (VarC "x"))
        (AppC
          (AppC (VarC "cons") (VarC "x"))
          (VarC "empty")))))),
  ("(forall (a b) (fun ([x : a] [y : b]) " ++
      "((cons <(a, b)>) ((pair <a b>) x y) (empty <(a, b)>))))",
    (FunC "x" (FunC "y" (AppC (AppC (VarC "cons")
      (AppC (AppC (VarC "pair") (VarC "x")) (VarC "y"))) (VarC "empty"))))),

  -- Taken from problem 5 tests from discussion board.
  ("(with* ([id (forall (a) (fun ([y : a]) y))] [selfapp (fun ([x : (forall a.(a -> a))]) ((x <(forall b.(b -> b))>) x))]) (selfapp id))",
    AppC (FunC "id"
      (AppC (FunC "selfapp" (AppC (VarC "selfapp") (VarC "id")))
        (FunC "x" (AppC (VarC "x") (VarC "x")))))
      (FunC "y" (VarC "y"))),
  ("(with* ([two (forall (a) (fun ([s : (a -> a)] [z : a]) (s (s z))))] " ++
           "[add (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                  "[m : ((a -> a) -> (a -> a))] " ++
                                  "[s : (a -> a)] " ++
                                  "[z : a]) ((n s) (m s z))))] " ++
           "[mult (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                   "[m : ((a -> a) -> (a -> a))] " ++
                                   "[s : (a -> a)]) (n (m s))))]) " ++
    "(((mult <num>) ((add <num>) (two <num>) (two <num>)) (two <num>)) (+ 1) 0))",
    AppC (FunC "two" (AppC (FunC "add" (AppC (FunC "mult"
      (AppC (AppC (AppC
            (AppC (VarC "mult") (AppC (AppC (VarC "add") (VarC "two")) (VarC "two")))
            (VarC "two"))
          (AppC (VarC "+") (NumC 1)))
        (NumC 0)))
      (FunC "n" (FunC "m" (FunC "s" (AppC (VarC "n") (AppC (VarC "m") (VarC "s"))))))))
      (FunC "n" (FunC "m" (FunC "s" (FunC "z"
        (AppC (AppC (VarC "n") (VarC "s")) (AppC (AppC (VarC "m") (VarC "s")) (VarC "z")))))))))
      (FunC "s" (FunC "z" (AppC (VarC "s") (AppC (VarC "s") (VarC "z"))))))
  ]

runProblem6Test (inputStr, expected) =
  do expr <- parseStr inputStr
     let actual = erase expr in case actual == expected of
       True -> Ok ()
       False -> Err ("Expected: " ++ (show expected) ++ "; Actual: " ++ (show actual) ++
         " --- In: " ++ (show inputStr))

runProblem6Tests =
  map runProblem6Test problem6Tests


interpTests = [
  ("5", Ok (NumV 5)),
  ("\"abc\"", Ok (StringV "abc")),
  ("true", Ok (BoolV True)),
  ("false", Ok (BoolV False)),
  ("(+ 5 3)", Ok (NumV 8)),
  ("(+ false)", Err "Function requires num but was called with bool"),
  ("(+ 5 false)", Err "Function requires num but was called with bool"),
  ("(if (= 5 3) 1 2)", Ok (NumV 2)),
  ("(if (= 5 3) false true)", Ok (BoolV True)),
  ("((fun ([x : num]) (* x x)) 5)", Ok (NumV 25)),
  ("((fun ([x : bool]) (if x false true)) true)", Ok (BoolV False)),
  ("((fun ([x : num] [y : num]) (if (< x y) x y)) 5 10)", Ok (NumV 5)),
  ("(((forall (a) (fun ([x : a]) ((cons <a>) x (empty <a>)))) <num>) 5)",
    Ok (ConsV (NumV 5) EmptyV)),
  ("(((forall (a) (fun ([x : a] [l : (list a)]) ((cons <a>) x l))) <bool>) " ++
      "true ((cons <bool>) false ((cons <bool>) true (empty <bool>))))",
    Ok (ConsV (BoolV True) (ConsV (BoolV False) (ConsV (BoolV True) EmptyV)))),
  ("(with* ([conscons " ++
      "(forall (a) (fun ([x : a]) {(cons <a>) x ((cons <a>) x (empty <a>))}))]) " ++
        "((conscons <num>) 5))",
    Ok (ConsV (NumV 5) (ConsV (NumV 5) EmptyV))),
  ("(with* ([ mkpair (forall (a b) (fun ([x : a] [y : b]) { (pair <a b>) x y }))]) " ++
      "((mkpair <bool num>) true 5))",
    Ok (PairV (BoolV True) (NumV 5))),
  ("(((forall (a b) (fun ([x : a] [y : b]) " ++
      "((cons <(a, b)>) ((pair <a b>) x y) (empty <(a, b)>)))) <num bool>) 5 true)",
    Ok (ConsV (PairV (NumV 5) (BoolV True)) EmptyV)),

  -- Taken from problem 5 tests from discussion board
  ("(((with* ([id (forall (a) (fun ([y : a]) y))] " ++
      "[selfapp (fun ([x : (forall a.(a -> a))]) " ++
      "((x <(forall b.(b -> b))>) x))]) (selfapp id)) <num>) 5)",
    Ok (NumV 5)),
  ("(with* ([two (forall (a) (fun ([s : (a -> a)] [z : a]) (s (s z))))] " ++
           "[add (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                  "[m : ((a -> a) -> (a -> a))] " ++
                                  "[s : (a -> a)] " ++
                                  "[z : a]) ((n s) (m s z))))] " ++
           "[mult (forall (a) (fun ([n : ((a -> a) -> (a -> a))] " ++
                                   "[m : ((a -> a) -> (a -> a))] " ++
                                   "[s : (a -> a)]) (n (m s))))]) " ++
    "(((mult <num>) ((add <num>) (two <num>) (two <num>)) (two <num>)) (+ 1) 0))",
    Ok (NumV 8))
  ]

runInterpTest (inputStr, expected) =
  let actual = interpStr inputStr in case (show actual) == (show expected) of
    True -> Ok ()
    False -> Err ("Expected: " ++ (show expected) ++ "; Actual: " ++ (show actual) ++
      " --- In: " ++ (show inputStr))

runInterpTests =
  map runInterpTest interpTests

runAllTests =
  [ runProblem1Tests,
    runProblem2Tests,
    runProblem3Tests,
    runProblem4Tests,
    runProblem5Tests,
    runProblem6Tests,
    runInterpTests
    ]
