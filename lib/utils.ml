open Core

(* functions to process sudokus from text files and helper functions *)
type t = int option array array

let box_corner_indices = Array.init 9 ~f:(fun i -> i / 3 * 3, i mod 3 * 3)

let box_indices (row, col) =
  let box_row = row / 3 * 3 and box_col = col / 3 * 3 in
  Array.init 9 ~f:(fun i -> box_row + i / 3, box_col + i mod 3)

let boxes_indices = Array.map ~f:box_indices box_corner_indices

let boxes array =
  Array.map ~f:(Array.map ~f:(fun (r, c) -> array.(r).(c))) boxes_indices

let rows_cols_present coords =
  let rows = Array.map coords ~f:fst
  and cols = Array.map coords ~f:snd in
  let uniques array = array
    |> Array.to_list
    |> List.dedup_and_sort ~compare:Int.compare
  in
  uniques rows, uniques cols

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

let divider = "+-------+-------+-------+"

let to_string (t : t) =
  let s = Buffer.create 256 in
  let print_horizontal_divider buffer =
    Buffer.add_string buffer divider;
    Buffer.add_char buffer '\n'
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

let of_string string : t option =
  let sudoku = Array.make_matrix ~dimx:9 ~dimy:9 None in
  let all_lines = String.split_lines string in
  let lines = List.filter all_lines ~f:(String.(<>) divider) in
  List.iteri lines ~f:(fun r line ->
    if String.(=) line divider then () else
    let (chars : char list) = Scanf.sscanf line
      "| %c %c %c | %c %c %c | %c %c %c |"
      (fun a b c d e f g h i -> [a; b; c; d; e; f; g; h; i]) in
    List.iteri chars ~f:(fun c char ->
      if Char.(<>) char '.' then
        sudoku.(r).(c) <- Some (int_of_char char - int_of_char '0')
    );
  );
  create sudoku

let read (filename : string) : t option =
  of_string (In_channel.read_all filename)

let write t (filename : string) : unit =
  let oc = Out_channel.create filename in
  Out_channel.output_string oc (to_string t);
  Out_channel.close oc

(* data structure used for solving the sudoku: a 2D array with a list of
   possible values for each cell *)
type possibilities = int list array array

let initial_possibilities (sudoku : t) : possibilities =
  let possibilites_from_spec = (function
    | None -> List.init 9 ~f:(fun x -> x + 1)
    | Some x -> [x])
  in
  Array.map ~f:(Array.map ~f:possibilites_from_spec) sudoku

let row_poss_coords possibilities row : int list array * (int * int) array =
  let row_poss = possibilities.(row) in
  let row_coords = Array.init 9 ~f:(fun i -> row, i) in
  row_poss, row_coords

let col_poss_coords cols_possibilities col : int list array * (int * int) array =
  let col_poss = cols_possibilities.(col) in
  let col_coords = Array.init 9 ~f:(fun i -> i, col) in
  col_poss, col_coords

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
