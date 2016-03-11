(** Phantom type for allocated pointer *)
type alloc = Alloc

(** Phantom type for freed/unallocated pointer *)
type free = Free

(** Type for pointers, equivalent to int64 for now *)
type ('a, _) ptr = int64

(** Dereference an allocated pointer and get its value *)
external deref : ('a, alloc) ptr -> 'a = "deref_stub"

(** Assign a new value to an allocated pointer *)
external assign : ('a, alloc) ptr -> 'a -> ('a, alloc) ptr = "assign_stub"

(** Perform pointer arithmetic by adding a specified offset to a pointer's address *)
external offset : ('a, 'status) ptr -> int64 -> ('a, 'status) ptr = "offset_stub"

(** Allocate a new pointer with the available `malloc` function *)
external alloc : int -> ('a, alloc) ptr = "alloc_stub"

(** Free an existing pointer with the available `free` function *)
external free  : ('a, alloc) ptr -> ('a, free) ptr = "free_stub"

(** Get a pointer to an OCaml value... very unsafe *)
external getref : 'a -> ('a, 'status) ptr = "getref_stub"

(** Call the available `puts` function with a char pointer *)
external puts : (char, alloc) ptr -> unit = "puts_stub"

(** Get the address of a pointer as an int64. Somewhat broken :^) *)
external address : ('a, 'status) ptr -> int64 = "%identity"

(** Get the pointer to an address in memory *)
external pointer : int64 -> ('a, 'status) ptr = "pointer_stub"

(** Like `assign`, but returns () *)
let mutate ptr value =
  ignore (assign ptr value)

(** Deref operator *)
let ( !* ) = deref

(** Mutate/assignment operator *)
let ( ^= ) = mutate

(** Offset/pointer arithmetic operator *)
let ( ^+ ) = offset

(** Referencing operator *)
let ( !& ) = getref

(** Run a function with a pointer, making sure it is deallocated by the end of the function *)
let with_ptr : ('a, alloc) ptr -> (('a, alloc) ptr -> unit) -> unit =
  fun ptr run ->
    run ptr;
    free ptr |> ignore

let encode_string : string -> (char, alloc) ptr =
  fun str ->
    let len = String.length str in
    let ptr = alloc (len + 1) in
    String.iteri begin fun i c ->
      let ptr' = ptr ^+ (Int64.of_int i) in
      ptr' ^= c
    end str;
    let last = ptr ^+ (Int64.of_int len) in
    last ^= 0;
    ptr
