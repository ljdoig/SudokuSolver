open Core

let solve_sudoku filename ~verbose =
  let open Option.Monad_infix in
  Sudoku.read filename
  >>= (fun sudoku ->
      let start_time = Time.now () in
      let output = Sudoku.solve ~verbose sudoku in
      let end_time = Time.now () in
      output >>| (fun solved ->
        let runtime_ms =
          Time.diff end_time start_time
          |> Time.Span.to_ms
          |> Float.to_int
        in
        (solved, runtime_ms)
      ))
  |> function
    | None -> print_endline "Illegal sudoku"
    | Some (solved, runtime_ms) ->
        Sudoku.print solved;
        Printf.printf "Solved in %dms\n" runtime_ms

let command =
  Command.basic
    ~summary:"Solve a sudoku from a textfile"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
    and verbose = flag "v" no_arg ~doc:" print verbose output while solving" in
    fun () -> solve_sudoku filename ~verbose)

let () = Command_unix.run ~version:"1.0" ~build_info:"sudoku" command
