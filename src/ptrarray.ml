open Ptr

module type ARRAY_CELL = sig
  type t
  val size : int
end

(** Unsized array, not necessarily null-terminated *)
module Array (Cell : ARRAY_CELL) = struct
  (** Array of values of type 'a *)
  type 'a array = ('a, alloc) ptr

  (** Create an uninitialized array of the specified length *)
  let create : int -> Cell.t array = fun length ->
    let size = Cell.size * length in
    alloc size

  (** Free the memory allocated by the array *)
  let delete : Cell.t array -> (Cell.t, free) ptr = free

  (** Get a pointer to the specified index of the array *)
  let index : Cell.t array -> int -> (Cell.t, alloc) ptr = fun arr i ->
    arr ^+ (Int64.of_int (i * Cell.size))

  (** Get the value at the specified index of the array *)
  let value : Cell.t array -> int -> Cell.t = fun arr i ->
    !*(index arr i)

  (** Set the value at the specified index of the array *)
  let assign : Cell.t array -> int -> Cell.t -> unit = fun arr i x ->
    (index arr i) ^= x

  (** Map a function over each item in the array *)
  let map : (Cell.t -> Cell.t) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      (index arr i) ^= f (value arr i)
    done

  (** Map a function over each item of the array, paying attention to the indices as it goes *)
  let mapi : (int -> Cell.t -> Cell.t) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      (index arr i) ^= f i (value arr i)
    done

  (** Iterate over each item of the array *)
  let iter : (Cell.t -> unit) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      f (value arr i)
    done

  (** Iterate over each item of the array, paying attention to the indices as it goes *)
  let iteri : (int -> Cell.t -> unit) -> int -> Cell.t array -> unit = fun f len arr ->
    for i = 0 to len - 1 do
      f i (value arr i)
    done

  (** Run a function with a vector, making sure it is deallocated by the end of the function *)
  let with_arr : ('a, alloc) ptr -> (('a, alloc) ptr -> unit) -> unit =
    fun arr run ->
      run arr;
      delete arr |> ignore
end

(** Sized array *)
module Vector (Cell : ARRAY_CELL) = struct
  (** Sized array of values of type 'a *)
  type 'a array = int * ('a, alloc) ptr

  (** Create an uninitialized array of the specified length *)
  let create : int -> Cell.t array = fun length ->
    let size = Cell.size * length in
    (length, alloc size)

  (** Get the length of an existing array *)
  let length : Cell.t array -> int = fun (length, _) -> length

  (** Free the memory allocated by the array *)
  let delete : Cell.t array -> (Cell.t, free) ptr = fun arr ->
    free (snd arr)

  (** Get a pointer to the specified index of the array *)
  let index : Cell.t array -> int -> (Cell.t, alloc) ptr = fun arr i ->
    (snd arr) ^+ (Int64.of_int (i * Cell.size))

  (** Get the value at the specified index of the array *)
  let value : Cell.t array -> int -> Cell.t = fun arr i ->
    !*(index arr i)

  (** Set the value at the specified index of the array *)
  let assign : Cell.t array -> int -> Cell.t -> unit = fun arr i x ->
    (index arr i) ^= x

  (** Map a function over each item in the array *)
  let map : (Cell.t -> Cell.t) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      (index arr i) ^= f (value arr i)
    done

  (** Map a function over each item of the array, paying attention to the indices as it goes *)
  let mapi : (int -> Cell.t -> Cell.t) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      (index arr i) ^= f i (value arr i)
    done

  (** Iterate over each item of the array *)
  let iter : (Cell.t -> unit) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      f (value arr i)
    done

  (** Iterate over each item of the array, paying attention to the indices as it goes *)
  let iteri : (int -> Cell.t -> unit) -> Cell.t array -> unit = fun f arr ->
    for i = 0 to length arr - 1 do
      f i (value arr i)
    done

  (** Run a function with a vector, making sure it is deallocated by the end of the function *)
  let with_vec : int * ('a, alloc) ptr -> (int * ('a, alloc) ptr -> unit) -> unit =
    fun vec run ->
      run vec;
      delete vec |> ignore
end
