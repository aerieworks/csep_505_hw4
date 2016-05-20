(with* [(mkrecur (fun (f) (f f)))
        (id (fun (v) v))
        (and (fun (a b) { if a b false }))
        (or (fun (a b) { if a true b }))
        (not (fun (a) { if a false true }))
        (xor (fun (a b) { or (and a (not b)) (and b (not a)) }))

        (- (fun (a b) { + a (* b -1) }))
        (> (fun (a b) { < b a }))
        (<= (fun (a b) { not (> a b) }))
        (>= (fun (a b) { not (< a b) }))

        (consify (fun (v) { cons v empty }))

        (continue (fun (v) { pair v true } ))
        (break (fun (v) { pair v false } ))
        (continue? (fun (cond con alt) { if (snd cond) (con (fst cond)) (alt (fst cond)) }))

        (while (mkrecur (fun (w)
                             { fun (v f)
                                   { with* [(result (f v))]
                                     (continue? result (fun (v') { w w v' f }) id)
                                     }
                                   })))

        (for (mkrecur (fun (_for)
                           { fun (start until v f)
                                 { if (< start until)
                                      (_for _for (+ start 1) until (f v start) f)
                                      v
                                      }
                                 })))

        (map (mkrecur (fun (_map)
                           { fun (src f)
                                 { with* [(v (f (first src)))]
                                   (if (empty? (rest src))
                                       (consify v)
                                       (cons v (_map _map (rest src) f)))
                                   }
                                 })))

        (factorial (mkrecur (fun (fact)
                                 { fun (x)
                                      { if (< x 2)
                                          (+ 0 1)
                                          (* x (fact fact (+ x -1)))
                                          }
                                      })))

        (head (for 1 10 (consify 0) (fun (v i) { cons i v })))
        (result (map head (fun (in) { factorial in })))
        ]
        (cons head result)
)
