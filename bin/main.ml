open Core

let solve_sudoku filename ~verbose =
  match Sudoku.read filename with
  | None -> print_endline "illegal sudoku"
  | Some sudoku ->
    if verbose then
      Sudoku.solve ~verbose sudoku |> ignore
    else
      begin
        let start_time = Time.now () in
        let solved = Sudoku.solve ~verbose sudoku in
        let end_time = Time.now () in
        Time.diff end_time start_time
          |> Time.Span.to_sec
          |> Printf.printf "Solving took: %f seconds\n";
        let open Option.Monad_infix in
        solved >>| Sudoku.print |> ignore
      end

let command =
  Command.basic
    ~summary:"Solve a sudoku from a textfile"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
    and verbose = flag "v" no_arg ~doc:" print verbose output while solving" in
    fun () -> solve_sudoku filename ~verbose)

let () = Command_unix.run ~version:"1.0" ~build_info:"sudoku" command
