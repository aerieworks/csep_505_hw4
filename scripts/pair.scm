(with* [(r ((pair <bool bool>) true false))
        (p ((pair <num (bool, bool)>)  5 r))]
       ((fst <bool bool>) ((snd <num (bool, bool)>) p))
)
