open Core

type t = int option array array
type possibilities = int list array array

let box_corner_indices = Array.init 9 ~f:(fun i -> i / 3 * 3, i mod 3 * 3)

let box_indices (row, col) =
  let box_row = row / 3 * 3 and box_col = col / 3 * 3 in
  Array.init 9 ~f:(fun i -> box_row + i / 3, box_col + i mod 3)

let boxes_indices = Array.map ~f:box_indices box_corner_indices

let boxes array = box_corner_indices
  |> Array.map ~f:(fun coords -> box_indices coords)
  |> Array.map ~f:(Array.map ~f:(fun (r, c) -> array.(r).(c)))

let cols array = Array.transpose_exn array

let create (array : int option array array) : t option =
  if Array.length array <> 9 then None else
  if Array.exists ~f:(fun row -> Array.length row <> 9) array then None else
  let invalid_cell = function
  | Some x -> not (1 <= x && x <= 9)
  | None -> false
  in
  if Array.exists ~f:(fun row -> Array.exists ~f:invalid_cell row) array then
    None
  else
  let no_duplicate x = x
    |> Array.filter_map ~f:Fn.id
    |> Array.sorted_copy ~compare:Int.compare
    |> Array.find_consecutive_duplicate ~equal:(=)
    |> is_none
  in
  let valid_rows = Array.for_all ~f:no_duplicate array in
  let valid_cols = Array.for_all ~f:no_duplicate (cols array) in
  let valid_boxes = Array.for_all ~f:no_duplicate (boxes array) in
  if valid_rows && valid_cols && valid_boxes then Some array else None

let to_string (t : t) =
  let s = Buffer.create 256 in
  let print_horizontal_divider buffer =
    Buffer.add_string buffer "+-------+-------+-------+\n"
  in
  Array.iteri
    ~f:(fun i row ->
      if i mod 3 = 0 then print_horizontal_divider s;
      Array.iteri
        ~f:(fun j cell ->
          if j mod 3 = 0 then Buffer.add_string s "| ";
          match cell with
          | Some x -> Buffer.add_string s (Printf.sprintf "%d " x)
          | None -> Buffer.add_string s ". ")
        row;
      Buffer.add_string s "|\n")
    t;
  print_horizontal_divider s;
  Buffer.contents s

let read (filename : string) : t option =
  let ic = In_channel.create filename in
  let sudoku = Array.make_matrix ~dimx:9 ~dimy:9 None in
  for r = 0 to 8 do
    if r mod 3 = 0 then ignore (In_channel.input_line ic);
    match In_channel.input_line ic with
    | None -> In_channel.close ic
    | Some line ->
      let (chars : char list) = Scanf.sscanf line
        "| %c %c %c | %c %c %c | %c %c %c |"
        (fun a b c d e f g h i -> [a; b; c; d; e; f; g; h; i]) in
      List.iteri chars ~f:(fun c x -> match x with
        | '.' -> ()
        | x -> sudoku.(r).(c) <- Some (int_of_char x - int_of_char '0'));
  done;
  create sudoku

let write t (filename : string) : unit =
  let oc = Out_channel.create filename in
  Out_channel.output_string oc (to_string t);
  Out_channel.close oc

let of_possibilities (possibilities : possibilities) : t option =
  let nums = Array.init 9 ~f:(fun i -> i + 1) in
  if Array.for_all nums ~f:(fun n ->
    let n_still_possible =
      Array.exists ~f:(fun cell_poss -> List.mem cell_poss ~equal:Int.equal n)
    in
    Array.for_all ~f:n_still_possible possibilities &&
    Array.for_all ~f:n_still_possible (cols possibilities) &&
    Array.for_all ~f:n_still_possible (boxes possibilities)) |> not
  then None else
  let array = Array.make_matrix ~dimx:9 ~dimy:9 None in
  Array.iteri
    ~f:(fun i row_possibilities ->
      Array.iteri
        ~f:(fun j cell_possibilities ->
          if List.length cell_possibilities = 1
          then array.(i).(j) <- Some (List.hd_exn cell_possibilities))
        row_possibilities)
    possibilities;
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
    | None -> print_endline "Invalid solution"
    | Some solved ->
      print_endline "Before: ";
      t |> to_string |> print_endline;
      print_endline "After: ";
      solved |> to_string |> print_endline;
      if Array.for_all ~f:(Array.for_all ~f:(Option.is_some)) solved
      then Printf.printf "Complete sudoku!\n";
