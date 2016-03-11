open Camlptr

let test1 () =
  print_endline "Allocating 1 byte";
  with_ptr (alloc 1) begin fun ptr ->
    !*ptr |> string_of_int |> print_endline;
    print_endline "Freeing 1 byte";
    free ptr
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
    puts ptr;
    free ptr
  end

let test3 () =
  let hello = "Hello, world!" in
  with_ptr (encode_string hello) begin fun encoded ->
    puts encoded;
    free encoded
  end

let test4 () =
  Random.self_init ();
  let num = Random.int 10 in
  print_endline (string_of_int num);
  let ptr = getref num in
  puts ptr

let test5 () =
  let module IntCell = struct
    type t = int
    let size = 4 (* bytes *)
  end in
  let module IntArray  = Array  (IntCell) in
  let module IntVector = Vector (IntCell) in

  let arr = IntArray.create 10 in
  IntArray.mapi (fun i _ -> i + 1) 10 arr;
  IntArray.iter (fun n -> print_endline (string_of_int n)) 10 arr;

  let vec = IntVector.create 10 in
  IntVector.mapi (fun i _ -> i + 1) vec;
  IntVector.iter (fun n -> print_endline (string_of_int n)) vec

let () =
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ()
