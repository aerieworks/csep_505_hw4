(with* ([f (fun ([x : num]) (+ x (raise "bad problem!")))])
  (call-with-handler
    (fun (_) (f 3))
    (fun (err) (pair "caught" err))))
