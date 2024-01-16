open Core

let solve_sudoku filename ~verbose =
  let open Option.Monad_infix in
  Sudoku.read filename
  >>= (fun sudoku ->
      let start_time = Time.now () in
      let output = Sudoku.solve ~verbose sudoku in
      let end_time = Time.now () in
      output >>| (fun solved ->
        let runtime = Time.diff end_time start_time |> Time.Span.to_sec in
        (solved, runtime)
      ))
  |> function
    | None -> print_endline "Illegal sudoku"
    | Some (solved, runtime) ->
        Sudoku.print solved;
        print_endline ("Solving took: " ^ string_of_float runtime)

let command =
  Command.basic
    ~summary:"Solve a sudoku from a textfile"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
    and verbose = flag "v" no_arg ~doc:" print verbose output while solving" in
    fun () -> solve_sudoku filename ~verbose)

let () = Command_unix.run ~version:"1.0" ~build_info:"sudoku" command
