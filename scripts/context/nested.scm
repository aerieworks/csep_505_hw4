(call-with-context
  5
  (fun (_)
       (call-with-context
         3
         (fun (_) ((fun (ctx) { pair (first ctx) (first (rest ctx)) }) (get-context _)))
         )
       )
  )

