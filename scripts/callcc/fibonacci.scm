(with* ([y (fun (f)
             ((fun (g) (f (fun (x) ((g g) x))))
              (fun (g) (f (fun (x) ((g g) x))))))]
        [fib (y (fun (fib a b yield/rtn)
                  (with* ([yield/rtn (call/cc (fun (k) (yield/rtn (pair a k))))])
                    (fib b (+ a b) yield/rtn))))]
        [iterate (y (fun (iterate n f x)
                      (if (= n 0)
                          x
                          (iterate (+ n -1) f (f x)))))])
  (iterate 20 (fun (p) (call/cc (snd p))) (pair 1 (fib 1 1))))
