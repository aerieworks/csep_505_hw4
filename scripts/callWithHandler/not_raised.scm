(+ 5
  (call-with-handler
    (fun (_) 3)
    (fun (err) { pair "caught error" err })))
