open Camlptr

let test1 () =
  print_endline "Allocating 1 byte";
  with_ptr (alloc 1) begin fun ptr ->
    !*ptr |> string_of_int |> print_endline;
    print_endline "Freeing 1 byte"
  end

let test2 () =
  print_endline "Allocating 10 char string";
  with_ptr (alloc 11) begin fun ptr ->
    for i = 0 to 9 do
      Printf.printf "On char %d\n%!" i;
      let off = Int64.of_int i in
      (ptr ^+ off) ^= 'a'
    done;
    (ptr ^+ 10L) ^= 0;
    puts ptr
  end

let test3 () =
  let hello = "Hello, world!" in
  with_ptr (encode_string hello) begin fun encoded ->
    puts encoded
  end

let test4 () =
  let module IntCell = struct
    type t = int
    let size = 4 (* bytes *)
  end in
  let module IntArray  = Array  (IntCell) in
  let module IntVector = Vector (IntCell) in

  IntArray.with_arr (IntArray.create 10) begin fun arr ->
    IntArray.mapi (fun i _ -> i + 1) 10 arr;
    IntArray.iter (fun n -> string_of_int n |> print_endline) 10 arr
  end;

  IntVector.with_vec (IntVector.create 10) begin fun vec ->
    IntVector.mapi (fun i _ -> i + 1) vec;
    IntVector.iter (fun n -> string_of_int n |> print_endline) vec
  end

let test5 () =
  let module StringCell = struct
    type t = (char, alloc) ptr
    let size = 8 (* bytes *)
  end in
  let module StringVector = Vector (StringCell) in
  StringVector.with_vec (StringVector.create 10) begin fun vec ->
    StringVector.mapi (fun i _ -> print_endline (string_of_int (i * StringCell.size));
                        encode_string "Hello, gov'na") vec; (* FIXME *)
    StringVector.iter puts vec;
    (*StringVector.map free vec Make sure that each string gets freed *)
  end

let () =
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ()
