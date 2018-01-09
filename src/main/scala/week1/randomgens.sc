trait Generator[+T] {
  self => // an alias for "this"

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(Generator.this.generate).generate // alternative to the self alias
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random

  override def generate: Int = rand.nextInt()
}

val intpairs = new Generator[(Int, Int)] {
  override def generate: (Int, Int) = (integers.generate, integers.generate)
}

// using for-comprehension and translate back to HOFs
val booleans20 = for (x <- integers) yield x > 0
val booleans21 = integers map { x => x > 0 }
val booleans22 = new Generator[Boolean] {
  override def generate: Boolean = ((x: Int) => x > 0) (integers.generate)
}
// simplify
val booleans23 = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

val intpairs20 = for {
  i <- integers
  j <- integers
} yield (i, j)
def pairs21[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
  x => u map { y => (x, y) }
}
// expand map
def pairs22[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
  x =>
    new Generator[(T, U)] {
      override def generate: (T, U) = (x, u.generate)
    }
}
// expand flatMap
def pairs23[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
  override def generate: (T, U) = new Generator[(T, U)] {
    override def generate: (T, U) = (t.generate, u.generate)
  }.generate
}
// simplify
def pairs24[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
  override def generate: (T, U) = (t.generate, u.generate)
}

/** Other generators' building blocks */
def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}
def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)
def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(math.abs(idx))

oneOf("red", "blue", "yellow").generate

// A list is either an empty list or a non-empty list.
def lists: Generator[List[Int]] = for {
  isEmpty <- booleans20
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail
lists.generate

/** A Tree Generator */
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = for {
  isLeaf <- booleans20
  t <- if (isLeaf) leafTree else innerTree
} yield t
def leafTree = single(Leaf(integers.generate))
def innerTree = single(Inner(leafTree.generate, leafTree.generate))
trees.generate

// complete solution with complex trees (inner of inners)
def trees2: Generator[Tree] = for {
  isLeaf <- booleans20
  t <- if (isLeaf) leafTree2 else innerTree2
} yield t
def leafTree2 = for {
  x <- integers
} yield Leaf(x)
def innerTree2 = for {
  l <- trees2
  r <- trees2
} yield Inner(l, r)
trees2.generate

def trees3: Generator[Tree] = for {
  isLeaf <- booleans20
  t <- if (isLeaf) leafTree else innerTree3
} yield t
def innerTree3 = single(Inner(trees3.generate, trees3.generate))
trees3.generate

// Using generators for tests; see tests for ScalaCheck property-based tests
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for (i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for " + value)
  }
  println("passed " + numTimes + " tests")
}

test(pairs24(lists, lists)) {
  case (xs, ys) => (xs ++ ys).length >= xs.length
}