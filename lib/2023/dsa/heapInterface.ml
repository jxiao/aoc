module type HEAP = sig
  type 'a heap

  val create : unit -> 'a heap
  val size : 'a heap -> int
  val push : 'a heap -> 'a -> 'a heap
  val peek : 'a heap -> 'a
  val peek_opt : 'a heap -> 'a option
  val pop : 'a heap -> 'a * 'a heap
  val pop_opt : 'a heap -> 'a option * 'a heap
end
