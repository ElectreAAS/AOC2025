module T = Domainslib.Task

module type DAY = sig
  val day : bool -> T.pool -> Eio.Buf_read.t -> int
end

let days : (module DAY) array =
  [|
    (module Day0);
    (module Day1);
    (module Day2);
    (module Day3);
    (module Day4);
    (module Day5);
    (module Day6);
    (module Day7);
    (module Day8);
  |]

let expected = [ 0; 6; 4174379265; 3121910778619; 43; 14; 3263827; 40; 40 ]
