(call-with-handler
  (+ 5)
  (fun (err) { pair "caught" err })
)
