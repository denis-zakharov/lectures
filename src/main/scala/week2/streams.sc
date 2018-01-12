/** Stream implementation is similar to a list but the tail is lazy evaluated
  * (call-by-name is applied)
  */
val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
Stream(1, 2, 3)
(1 to 1000).toStream

def listRange(lo: Int, hi: Int): List[Int] =
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)

def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

// List operations applied to streams
Stream(1) #:: xs // a cons-operator for streams
