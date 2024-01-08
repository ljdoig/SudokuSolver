let () =
  match Sudoku.read "sudokus/example.txt" with
  | None -> print_endline "illegal sudoku"
  | Some sudoku -> Sudoku.print sudoku
  (* match Sudoku.solve sudoku with
  | None -> print_endline "No solution"
  | Some solved_sudoku -> Sudoku.print solved_sudoku *)
