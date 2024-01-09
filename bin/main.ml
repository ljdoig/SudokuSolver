open Core

let solve_sudoku filename =
  match Sudoku.read filename with
  | Some sudoku -> Sudoku.solve sudoku
  | None -> print_endline "illegal sudoku"

let command =
  Command.basic
    ~summary:"Solve a sudoku from a textfile"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
     in
     fun () -> solve_sudoku filename)

let () = Command_unix.run ~version:"1.0" ~build_info:"sudoku" command
