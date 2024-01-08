let () =
  match Sudoku.read "sudokus/unsolveable1.txt" with
  | None -> print_endline "illegal sudoku"
  | Some sudoku -> Sudoku.print sudoku; Sudoku.solve sudoku
