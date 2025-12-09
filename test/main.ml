open Eio.Std
module P = Eio.Executor_pool

let all fs pool =
  List.mapi
    (fun i expected ->
      ( Printf.sprintf "Day %d" i,
        `Quick,
        fun () ->
          Utils.get_test fs i (fun input_buffer ->
              let (module Day) = All.days.(i) in
              let result = Day.day true pool input_buffer in
              Alcotest.(check int)
                "puzzle input should be solved!" expected result) ))
    All.expected

let () =
  Eio_main.run @@ fun env ->
  Random.self_init ();
  Switch.run @@ fun sw ->
  let pool =
    P.create ~sw
      ~domain_count:(Domain.recommended_domain_count () - 1)
      (Eio.Stdenv.domain_mgr env)
  in
  Alcotest.run "Everything" [ ("Test puzzle input", all env#fs pool) ]
