(+ (with* ([f (call-with-handler
               (fun (_) (fun (x) (+ x (raise "really bad problem!"))))
               (fun (err) (pair "caught" err)))])
          (f 3)))
