// IntSet as a tree structure
abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false

  def incl(x: Int) = NonEmpty(x, Empty, Empty)

  def union(other: IntSet) = other
}

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) =
    if (x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    else this

  def contains(x: Int) =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet) = (left union (right union other)) incl elem
}
// "LAWS"
val x = 10
val y = 20
val z = 30
val s = NonEmpty(1,
  NonEmpty(0, Empty, Empty),
  NonEmpty(2, NonEmpty(3, Empty, Empty), Empty))
Empty contains x      // false
(s incl x) contains x // true
(s incl x) contains y // s contains y

val otherSet = NonEmpty(10,
  NonEmpty(0, Empty, NonEmpty(3, Empty, Empty)),
  NonEmpty(20, NonEmpty(15, Empty, Empty), Empty))

//true
(s union otherSet) contains x
(s contains x) || (otherSet contains x)
//false
(s union otherSet) contains z
(s contains z) || (otherSet contains z)
