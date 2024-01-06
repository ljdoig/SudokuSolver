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

type posibilities = int list array array

let possibilities_to_string posibilities =
  let s = Buffer.create 256 in
  let print_horizontal_divider buffer =
    Buffer.add_string buffer "+-------+-------+-------+\n"
  in
  Array.iteri
    ~f:(fun i row ->
      if i mod 3 = 0 then print_horizontal_divider s;
      Array.iteri
        ~f:(fun j cell_possibilities ->
          if j mod 3 = 0 then Buffer.add_string s "| ";
          cell_possibilities
            |> List.map ~f:Int.to_string
            |> String.concat ~sep:""
            |> Buffer.add_string s;
        )
        row;
      Buffer.add_string s "|\n")
    posibilities;
  print_horizontal_divider s;
  Buffer.contents s

let initial_posibilities (sudoku : t) : posibilities =
  let possibilites_from_spec = (function
    | None -> List.init 9 ~f:(fun x -> x + 1)
    | Some x -> [x])
  in
  Array.map ~f:(Array.map ~f:possibilites_from_spec) sudoku

let print t =
  t |> to_string |> print_endline;
  t |> initial_posibilities |> possibilities_to_string |> print_endline
