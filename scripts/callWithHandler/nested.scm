(call-with-handler
  (fun (_) { pair (call-with-handler
       (fun (_) { raise "uhoh!" })
       (fun (err) { raise (pair "bigger" err) }))
     })
  (fun (err) { pair "caught" err })
  )
