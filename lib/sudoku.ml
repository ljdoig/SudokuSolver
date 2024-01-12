open Core

include Utils

(* logic for solving a sudoku *)

type possibilities = int list array array
let of_possibilities (possibilities : possibilities) : t option =
  if Array.exists possibilities ~f:(Array.exists ~f:List.is_empty) then None
  else
  let nums = Array.init 9 ~f:(fun i -> i + 1) in
  if Array.exists nums ~f:(fun n ->
    let n_not_possible = Array.for_all ~f:(
      fun cell_poss -> List.mem cell_poss ~equal:Int.equal n |> not
    ) in
    Array.exists ~f:n_not_possible possibilities &&
    Array.exists ~f:n_not_possible (cols possibilities) &&
    Array.exists ~f:n_not_possible (boxes possibilities)
  ) then None else
  let array = Array.make_matrix ~dimx:9 ~dimy:9 None in
  Array.iteri possibilities ~f:(fun i row_possibilities ->
    Array.iteri row_possibilities ~f:(fun j cell_possibilities ->
      match cell_possibilities with
      | [only_possibility] -> array.(i).(j) <- Some only_possibility
      | _ -> ()));
  create array

let possibilities_to_string possibilities =
  let s = Buffer.create 256 in
  let print_horizontal_divider buffer =
    for _ = 0 to 2 do
      Buffer.add_char buffer '+';
      Buffer.add_string buffer (String.make (9 * 3 + 2) '-');
    done;
    Buffer.add_string buffer "+\n";
  in
  Array.iteri
    ~f:(fun i row_possibilities ->
      if i mod 3 = 0 then print_horizontal_divider s;
      Array.iteri
        ~f:(fun j cell_possibilities ->
          Buffer.add_string s (if j mod 3 = 0 then "|" else " ");
          let possible = Array.init 9 ~f:(Fn.const ".") in
          List.iter cell_possibilities ~f:(
            fun n -> possible.(n - 1) <- Int.to_string n
          );
          possible
            |> String.concat_array ~sep:""
            |> Buffer.add_string s;
        )
        row_possibilities;
      Buffer.add_string s "|\n")
    possibilities;
  print_horizontal_divider s; Buffer.contents s

let initial_possibilities (sudoku : t) : possibilities =
  let possibilites_from_spec = (function
    | None -> List.init 9 ~f:(fun x -> x + 1)
    | Some x -> [x])
  in
  Array.map ~f:(Array.map ~f:possibilites_from_spec) sudoku

let constrain (possibilities : possibilities) progress : unit =
  let remove_possibility row col value =
    if List.mem possibilities.(row).(col) ~equal:Int.equal value then (
      possibilities.(row).(col) <- List.filter possibilities.(row).(col) ~f:(
        fun x -> x <> value
      );
      Printf.printf "(%d, %d) can't be %d\n" row col value;
      progress := true
    )
  in
  let constrain_row_and_col row col value =
    for i = 0 to 8 do
      if i <> col then
        remove_possibility row i value;
      if i <> row then
        remove_possibility i col value;
      done;
  and constrain_box row col value =
    Array.iter (box_indices (row, col)) ~f:(fun (row', col') ->
      if row' <> row || col' <> col then
        remove_possibility row' col' value
    )
  in
  (for row = 0 to 8 do
    for col = 0 to 8 do
      match possibilities.(row).(col) with
      | [value] ->
        constrain_row_and_col row col value;
        constrain_box row col value;
      | _ -> ()
      done;
    done;)
let assign (possibilities : possibilities) progress =
  let trim possibilities_set coords =
    let mem value list = List.mem list ~equal:Int.equal value in
    for value = 1 to 9 do
      if Array.count possibilities_set ~f:(mem value) = 1 then
        Array.iter2_exn possibilities_set coords ~f:(fun cell_poss (row, col) ->
          if mem value cell_poss && List.length cell_poss > 1
          then (
            possibilities.(row).(col) <- [value];
            progress := true;
            Printf.printf
              "(%d, %d) must be %d, previous possibilities: %s\n"
              row col value (List.to_string cell_poss ~f:Int.to_string)
          ))
      done
  in
  let boxes_possibilities = boxes possibilities in
  for i = 0 to 8 do
    let row_possibilities = possibilities.(i) in
    let col_possibilities = Array.map ~f:(fun r -> r.(i)) possibilities in
    trim row_possibilities (Array.init 9 ~f:(fun c -> (i, c)));
    trim col_possibilities (Array.init 9 ~f:(fun r -> (r, i)));
    trim boxes_possibilities.(i) boxes_indices.(i)
    done

let solve_internal possibilities : possibilities =
  let constrain_progress = ref true in
  let assign_progress = ref true in
  while !constrain_progress || !assign_progress do
    constrain_progress := false;
    constrain possibilities constrain_progress;
    if !constrain_progress then
      possibilities
        |> possibilities_to_string
        |> print_endline;
    assign_progress := false;
    assign possibilities assign_progress;
    if !assign_progress then
      possibilities
        |> possibilities_to_string
        |> print_endline;
  done;
  possibilities

let print t = t |> to_string |> print_endline

let solve t =
  t |> initial_possibilities
    |> solve_internal
    |> of_possibilities
    |> function
    | None -> print_endline "Sudoku specification produced invalid output"; None
    | Some solved ->
      print_endline "Before: ";
      t |> to_string |> print_endline;
      print_endline "After: ";
      solved |> to_string |> print_endline;
      if Array.for_all ~f:(Array.for_all ~f:Option.is_some) solved
      then Printf.printf "Complete sudoku!\n";
      Some solved
