(call-with-context
  5
  (fun (_) { (fun (x) { * 2 x }) (first (get-context _)) })
)
