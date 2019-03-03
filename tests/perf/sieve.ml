let rec filter p = parser
  [< 'n; s >] -> if p n then [< 'n; filter p s >] else [< filter p s >]
  
let naturals =
  let rec gen n = [< 'n; gen (succ n) >] in gen 2
  
let primes =
  let rec sieve = parser
    [< 'n; s >] -> [< 'n; sieve (filter (fun m -> m mod n <> 0) s) >] in
  sieve naturals
  
let () =
  for i = 1 to 3000 do
    ignore (Stream.next primes)
  done
