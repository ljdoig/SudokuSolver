open Core

include Utils

(* logic for solving a sudoku *)

(* basic data structure for solving - a 2D array where each entry is a list of
   possible values that can fill the corresponding cell *)
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
  Buffer.add_string s "      ";
  for i = 0 to 8 do
    Buffer.add_string s (Int.to_string i);
    Buffer.add_string s "         "
  done;
  Buffer.add_char s '\n';
  let print_horizontal_divider buffer =
    Buffer.add_char buffer ' ';
    for _ = 0 to 2 do
      Buffer.add_char buffer '+';
      Buffer.add_string buffer (String.make (9 * 3 + 2) '-');
    done;
    Buffer.add_string buffer "+\n";
  in
  Array.iteri
    ~f:(fun i row_possibilities ->
      if i mod 3 = 0 then print_horizontal_divider s;
      Buffer.add_string s (Int.to_string i);
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
  print_horizontal_divider s;
  Buffer.contents s

let initial_possibilities (sudoku : t) : possibilities =
  let possibilites_from_spec = (function
    | None -> List.init 9 ~f:(fun x -> x + 1)
    | Some x -> [x])
  in
  Array.map ~f:(Array.map ~f:possibilites_from_spec) sudoku

(* More expensive constraints to check after the obvious ones *)
let constrain_advanced possibilities remove_possibility verbose =
  let equal_by_coords (_ , (r1, c1)) (_, (r2, c2)) = (r1 = r2 && c1 = c2) in
  (* assumes that the two poss_coords sets contain the same values*)
  let remove_impossible poss_coords1 poss_coords2 =
    for value = 1 to 9 do
      Array.iter poss_coords1 ~f:(fun (_, (r, c)) ->
        (* if value is impossible in coords2, then it's impossible in coords1 *)
        if not (Array.exists poss_coords2 ~f:(fun (cell_poss2, _) ->
          List.mem cell_poss2 ~equal:Int.equal value
        ))
        then remove_possibility r c value
      )
    done
  in
  (* an overlapping box and row/col must have the same values in non-overlap *)
  let constrain_overlapping poss1 coords1 poss2 coords2 = (
    (* find entries in poss_coords1 that are not in poss_coords2 *)
    let diff poss_coords1 poss_coords2 = Array.filter poss_coords1 ~f:(
      fun entry -> not (Array.mem poss_coords2 entry ~equal:equal_by_coords)
    )
    in
    let poss_coords1 = Array.zip_exn poss1 coords1
    and poss_coords2 = Array.zip_exn poss2 coords2 in
    let diff1 = diff poss_coords1 poss_coords2
    and diff2 = diff poss_coords2 poss_coords1 in
    remove_impossible diff1 diff2;
    remove_impossible diff2 diff1
  )
  in
  (* constrain each possible box * row pair and each box * col pair as above *)
  let boxes_poss = boxes possibilities in
  let cols_possibilities = cols possibilities in
  Array.iter2_exn boxes_poss boxes_indices ~f:(
    fun box_poss box_coords ->
      let rows_present, cols_present = rows_cols_present box_coords in
      List.iter rows_present ~f:(fun row ->
        let row_poss, row_coords = row_poss_coords possibilities row in
        constrain_overlapping box_poss box_coords row_poss row_coords;
        constrain_overlapping row_poss row_coords box_poss box_coords
      );
      List.iter cols_present ~f:(fun col ->
        let col_poss, col_coords = col_poss_coords cols_possibilities col in
        constrain_overlapping box_poss box_coords col_poss col_coords;
        constrain_overlapping col_poss col_coords box_poss box_coords
      )
  );
  (* TODO: the 16 tiles found from the 2x2 boxes in each corner of the sudoku must
     match the 16 tiles that border the centre box, explained here:
     https://www.youtube.com/watch?v=pezlnN4X52g
  *)

  (* in any box/row/col if n cells have only n distinct possibilities between
     them then the remainin 9 - n cells can't contain any such values *)
  let check_n_distinct_in_n_cells poss coords =
    let all_poss_coords =
      List.zip_exn (Array.to_list poss) (Array.to_list coords)
    in
    (* unsolved cells *)
    let poss_coords = List.filter all_poss_coords ~f:(
      fun (poss, _) -> List.length poss > 1
    )
    in
    let rec get_subsets = function
      | [] -> [[]]
      | x :: xs ->
          let without_x = get_subsets xs in
          let with_x = List.map without_x ~f:(fun subset -> x :: subset) in
          without_x @ with_x
    in
    let subsets = List.filter (get_subsets poss_coords) ~f:(fun subset ->
      let length = List.length subset in
      1 < length && length < List.length poss_coords
    )
    in
    List.iter subsets ~f:(fun subset ->
      let unique_poss = subset
        |> List.concat_map ~f:fst
        |> List.dedup_and_sort ~compare:Int.compare
      in
      if List.length unique_poss = List.length subset then (
        if verbose then
          begin
            Printf.printf
              "Found possibilities %s in cells:"
              (List.to_string unique_poss ~f:Int.to_string);
            List.iter subset ~f:(
                fun (_, (r, c)) -> Printf.printf " (%d, %d)" r c
            );
            print_endline ""
          end;
        let rest = List.filter poss_coords ~f:(
          fun entry -> not (List.mem subset entry ~equal:equal_by_coords)
        )
        in
        List.iter rest ~f:(fun (_, (r, c)) ->
          List.iter unique_poss ~f:(fun value ->
            remove_possibility r c value
          )
        )
      )
    )
  in
  Array.iter2_exn ~f:check_n_distinct_in_n_cells boxes_poss boxes_indices;
  for i = 0 to 8 do
    let row_poss, row_coords = row_poss_coords possibilities i in
    check_n_distinct_in_n_cells row_poss row_coords;
    let col_poss, col_coords = col_poss_coords cols_possibilities i in
    check_n_distinct_in_n_cells col_poss col_coords;
  done


(* Enforces basic constraints. If a tile is filled with a certain value then the
   rest of its box/row/col can't contain that value. If that doesn't yield
   anything then try constrain_advanced. *)
let constrain ~verbose (possibilities : possibilities) progress : unit =
  let remove_possibility row col value =
    if List.mem possibilities.(row).(col) ~equal:Int.equal value then (
      possibilities.(row).(col) <- List.filter possibilities.(row).(col) ~f:(
        fun x -> x <> value
      );
      progress := true;
      if verbose then
        Printf.printf "(%d, %d) can't be %d\n" row col value
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
  done;);
  if not !progress then
    begin
      if verbose then print_endline "Trying advanced constraints";
      constrain_advanced possibilities remove_possibility verbose
    end

(* If only a single cell in a given box/row/cell can contain a certain value,
   then the cell must contain that value, so we can ignore all other values. *)
let assign ~verbose (possibilities : possibilities) progress =
  let trim possibilities_set coords =
    let mem value list = List.mem list ~equal:Int.equal value in
    for value = 1 to 9 do
      if Array.count possibilities_set ~f:(mem value) = 1 then
        Array.iter2_exn possibilities_set coords ~f:(fun cell_poss (row, col) ->
          if mem value cell_poss && List.length cell_poss > 1
          then (
            possibilities.(row).(col) <- [value];
            progress := true;
            if verbose then
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

let solve_internal ~verbose possibilities : possibilities =
  let constrain_progress = ref true in
  let assign_progress = ref true in
  while !constrain_progress || !assign_progress do
    constrain_progress := false;
    constrain ~verbose possibilities constrain_progress;
    if !constrain_progress && verbose then
      possibilities
        |> possibilities_to_string
        |> print_endline;
    assign_progress := false;
    assign ~verbose possibilities assign_progress;
    if !assign_progress && verbose then
      possibilities
        |> possibilities_to_string
        |> print_endline;
  done;
  possibilities

let print t = t |> to_string |> print_endline

let solve ?(verbose=false) t =
  t |> initial_possibilities
    |> solve_internal ~verbose
    |> of_possibilities
    |> (fun sudoku -> if verbose then (
          match sudoku with
          | None -> print_endline "Sudoku specification produced invalid output"
          | Some solved ->
              begin
                print_endline "Before: ";
                t |> to_string |> print_endline;
                print_endline "After: ";
                solved |> to_string |> print_endline;
                if Array.for_all ~f:(Array.for_all ~f:Option.is_some) solved
                then print_endline "Complete sudoku!";
              end);
        sudoku)
