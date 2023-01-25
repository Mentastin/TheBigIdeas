interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave u [] = u
interleave [] v = v
interleave (u:us) (v:vs) = [u] ++ [v] ++ (interleave us vs)