def expr = {
  val x = { print("X"); 1 } // strict evaluation
  lazy val y = { print("Y"); 2 } // lazy eval; first time of access
  def z = { print("Z"); 3 } // by-name; each time
  z + y + x + z + y + x
}
expr // prints "XZYZ"

def from(n: Int): Stream[Int] = n #:: from(n + 1)
val nats = from(0)
val m4s = nats map (_ * 4)
m4s.take(10).toList

// Prime Numbers using The Sieve of Eratosthenes
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))
val primes = sieve(from(2))
primes.take(100).toList

// Square root finding as a Stream
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

(sqrtStream(2) take 10).toList
sqrtStream(3) filter (isGoodEnough(_, 3))
