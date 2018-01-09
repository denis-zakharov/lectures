/** Writing a properties that are assumed to hold (pre- and post-conditions)
  * with ScalaCheck tool (also as a part of ScalaTest) */
import org.scalacheck.Prop.forAll
forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ++ l2).size
}.check
