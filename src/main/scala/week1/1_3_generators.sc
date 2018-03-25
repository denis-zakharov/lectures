trait Generator[+T] {
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate).generate
  }
}

// duplicate code
val integers = new Generator[Int] {
  val rand = new java.util.Random

  override def generate: Int = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  override def generate: (Int, Int) = (integers.generate, integers.generate)
}

// generic
val bools = for (x <- integers) yield x > 0
val ps = for {
  x <- integers
  y <- integers
} yield (x, y)

// other building blocks
def single[T](x: T) = new Generator[T] {
  override def generate: T = x
}

def choose[T](lo: Int, hi: Int) = for (x <- integers) yield lo + x % (hi - lo)

// vararg T*
def oneOf[T](xs: T*) = for (idx <- choose(0, xs.length)) yield xs(idx)

def lists: Generator[List[Int]] = for {
  isEmpty <- bools
  list <- if (isEmpty) single(Nil) else nonEmptyList
} yield list

def nonEmptyList: Generator[List[Int]] = for {
  head <- integers
  tail <- lists
} yield head::tail

integers.flatMap(i => lists.map(t => i::t))
lists.generate

// Tree generator
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = for {
  isLeaf <- bools
  tree <- if (isLeaf) single(Leaf(integers.generate)) else nonEmptyTree
} yield tree

def nonEmptyTree: Generator[Tree] = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

trees.generate

// Random Test Function
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for " + value)
  }
  println("Passed "+numTimes+" tests")
}

