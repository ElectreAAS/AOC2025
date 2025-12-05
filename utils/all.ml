module T = Domainslib.Task

module type DAY = sig
  val day : bool -> T.pool -> Eio.Buf_read.t -> string
end

let days : (module DAY) array =
  [| (module Day0); (module Day1); (module Day2); (module Day3) |]

let expected = [ ""; "6"; "4174379265"; "357" ]
