open Ptr

module type ARRAY_CELL = sig
  type t
  val size : int
end

(** Unsized array, not necessarily null-terminated *)
module Array (Cell : ARRAY_CELL) = struct
  type 'a array = ('a, alloc) ptr

  let create : int -> Cell.t array = fun length ->
    let size = Cell.size * length in
    alloc size

  let delete : Cell.t array -> (Cell.t, free) ptr = free

  let index : Cell.t array -> int -> (Cell.t, alloc) ptr = fun arr i ->
    arr ^+ (Int64.of_int (i * Cell.size))

  let value : Cell.t array -> int -> Cell.t = fun arr i ->
    !*(index arr i)

  let assign : Cell.t array -> int -> Cell.t -> unit = fun arr i x ->
    (index arr i) ^= x

 let map : (Cell.t -> Cell.t) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      (index arr i) ^= f (value arr i)
    done

  let mapi : (int -> Cell.t -> Cell.t) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      (index arr i) ^= f i (value arr i)
    done

  let iter : (Cell.t -> unit) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      f (value arr i)
    done

  let iteri : (int -> Cell.t -> unit) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      f i (value arr i)
    done
end

(** Sized array *)
module Vector (Cell : ARRAY_CELL) = struct
  type 'a array = int * ('a, alloc) ptr

  let create : int -> Cell.t array = fun length ->
    let size = Cell.size * length in
    (length, alloc size)

  let length : Cell.t array -> int = fun (length, _) -> length

  let delete : Cell.t array -> (Cell.t, free) ptr = fun arr ->
    free (snd arr)

  let index : Cell.t array -> int -> (Cell.t, alloc) ptr = fun arr i ->
    (snd arr) ^+ (Int64.of_int (i * Cell.size))

  let value : Cell.t array -> int -> Cell.t = fun arr i ->
    !*(index arr i)

  let assign : Cell.t array -> int -> Cell.t -> unit = fun arr i x ->
    (index arr i) ^= x

  let map : (Cell.t -> Cell.t) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      (index arr i) ^= f (value arr i)
    done

  let mapi : (int -> Cell.t -> Cell.t) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      (index arr i) ^= f i (value arr i)
    done

  let iter : (Cell.t -> unit) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      f (value arr i)
    done

  let iteri : (int -> Cell.t -> unit) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      f i (value arr i)
    done
end
