open Core

let solve_sudoku filename =
  match Sudoku.read filename with
  | Some sudoku ->
    begin
      let start_time = Time.now () in
      Sudoku.solve ~verbose:true sudoku |> ignore;
      let end_time = Time.now () in
      Time.diff end_time start_time
        |> Time.Span.to_sec
        |> Printf.printf "Solving took: %f seconds\n"
    end
  | None -> print_endline "illegal sudoku"

let command =
  Command.basic
    ~summary:"Solve a sudoku from a textfile"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
     in
     fun () -> solve_sudoku filename)

let () = Command_unix.run ~version:"1.0" ~build_info:"sudoku" command
