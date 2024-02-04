module Interpreter

open P

let interpret str =
    str
    |> scan
    |> printf "%A"