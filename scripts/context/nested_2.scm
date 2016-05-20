(with* ([f (fun (x) (pair x (get-context 12345)))])
  (call-with-context
   "there"
   (fun (_) (call-with-context
             "hi"
             (fun (_) (f true))))))
