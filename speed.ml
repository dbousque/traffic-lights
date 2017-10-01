

let rec until acc = function
  | 0 -> acc
  | n -> until (acc + 1) (n - 1)

let () =
  print_int (until 0 2 000 000 000)
