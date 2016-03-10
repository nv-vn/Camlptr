type alloc = Alloc
type free = Free

type _ ptr = int64

external deref : alloc ptr -> 'a = "deref_stub"
external assign : alloc ptr -> 'a -> alloc ptr = "assign_stub"
external offset : 'a ptr -> int64 -> 'a ptr = "offset_stub"
external alloc : int -> alloc ptr = "alloc_stub"
external free  : alloc ptr -> free ptr = "free_stub"
external getref : 'a -> 'b ptr = "getref_stub"

external puts : alloc ptr -> unit = "puts_stub"

external address : alloc ptr -> int64 = "%identity"
external pointer : int64 -> alloc ptr = "%identity"

let ( !* ) = deref
let ( ^= ) = assign
let ( ^+ ) = offset
let ( !& ) = getref

let with_ptr : alloc ptr -> (alloc ptr -> free ptr) -> unit =
  fun ptr run ->
    run ptr |> ignore

let encode_string : string -> alloc ptr =
  fun str ->
    let len = String.length str in
    let ptr = alloc (len + 1) in
    String.iteri begin fun i c ->
      let ptr' = offset ptr (Int64.of_int i) in
      assign ptr' c |> ignore
    end str;
    let last = offset ptr (Int64.of_int len) in
    assign last 0 |> ignore;
    ptr
