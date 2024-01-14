open OUnit2

let test_sudoku_solve (filestem : string) : unit =
  let in_file = filestem ^ "_in.txt" in
  let out_file = filestem ^ "_out.txt" in
  match Sudoku.read in_file, Sudoku.read out_file with
  | Some in', Some out -> assert_equal (Some out) (Sudoku.solve in')
  | _ -> print_endline "Invalid sudoku in one or both input files"; assert false

let test_sudoku_invalid (filestem : string) : unit =
  let in_file = filestem ^ "_in.txt" in
  match Sudoku.read in_file with
  | Some in' -> assert_equal None (Sudoku.solve in')
  | _ -> print_endline "Invalid sudoku in input file"; assert false

let suite =
  "Sudoku Tests" >::: [
    "Unsolveable 1" >:: (fun _ -> test_sudoku_invalid "unsolveable1");
    "Unsolveable 2" >:: (fun _ -> test_sudoku_invalid "unsolveable2");
    "Solveable 1" >:: (fun _ -> test_sudoku_solve "solveable1");
    "Solveable 2" >:: (fun _ -> test_sudoku_solve "solveable2");
    "Solveable 3" >:: (fun _ -> test_sudoku_solve "solveable3");
  ]

let () =
  Sys.chdir "../../../test/test_cases";
  run_test_tt_main suite
