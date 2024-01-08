open Core

type t = int option array array

let create (array : int option array array) : t option =
  let invalid_cell = function
  | Some x -> not (1 <= x && x <= 9)
  | None -> false
  in
  match Array.exists ~f:(fun row -> Array.exists ~f:invalid_cell row) array with
  | true -> None
  | false -> Some array

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

(*
let potential_assignment sudoku row col num =
  let same_num x = (x = Some num) in
  let row_nums = sudoku.(row) in
  let col_nums = Array.map (fun x -> x.(col)) sudoku in
  let box_row = row / 3 * 3 and box_col = col / 3 * 3 in
  let box_rows = Array.sub sudoku box_row 3 in
  let box = Array.map (fun row -> Array.sub row box_col 3) box_rows in
  row_nums :: col_nums :: Array.to_list box
    |> List.for_all (fun nums -> not (Array.exists same_num nums)) *)

type possibilities = int list array array

let box_corner_indices = Array.init 9 ~f:(fun i -> i / 3 * 3, i mod 3 * 3)

let () =
  Array.to_list box_corner_indices
    |> List.map ~f:(fun (i, j) -> sprintf "(%d, %d)" i j)
    |> String.concat ~sep:", "
    |> print_endline

let box_indices (row, col) =
  let box_row = row / 3 * 3 and box_col = col / 3 * 3 in
  Array.init 9 ~f:(fun i -> box_row + i / 3, box_col + i mod 3)

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
  for value = 1 to 9 do
    let contains_value list = List.mem list ~equal:Int.equal value in
    let trim possibilities_set coords =
      if Array.count possibilities_set ~f:contains_value = 1 then
        Array.iter2_exn possibilities_set coords ~f:(fun cell_poss (row, col) ->
          if contains_value cell_poss && List.length cell_poss > 1
          then (
            possibilities.(row).(col) <- [value];
            progress := true;
            Printf.printf
              "(%d, %d) must be %d, previous possibilities: %s\n"
              row col value (List.to_string cell_poss ~f:Int.to_string)
          ))
    in
    for i = 0 to 8 do
      let row_possibilities = possibilities.(i) in
      let col_possibilities = Array.map ~f:(fun r -> r.(i)) possibilities in
      trim row_possibilities (Array.init 9 ~f:(fun c -> (i, c)));
      trim col_possibilities (Array.init 9 ~f:(fun r -> (r, i)));
      let current_box_indices = box_indices box_corner_indices.(i) in
      let box_possibilities =
        Array.map ~f:(fun (r, c) -> possibilities.(r).(c)) current_box_indices
      in
      trim box_possibilities current_box_indices
      done
  done

let solve possibilities : unit =
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
  done

let print t =
  t |> to_string |> print_endline;
  t |> initial_possibilities
    |> solve
