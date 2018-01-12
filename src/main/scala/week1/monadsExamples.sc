import scala.util.control.NonFatal

val listMonad = List('a', 'b', 'c')
val listMonad2 = List('d', 'e', 'f')
val listMonad3 = List('g', 'h', 'i')
def fUpperChar(x: Char) = x.toUpper
def gCharCode(x: Char) = x.toOctalString

// Associative law // It justifies nested for-expressions
(listMonad flatMap (x => List(fUpperChar(x)))) flatMap (x => gCharCode(x))
listMonad flatMap (x => List(fUpperChar(x)) flatMap (x => gCharCode(x)))

// Left unit law // It justifies a trivial for (x <- m) yield x == m
List('a') flatMap (x => List(fUpperChar(x)))
List(fUpperChar('a')) // f(x) where x='a'

// Right unit law //
listMonad flatMap (x => List(x)) // m flatMap unit is the monad itself


// Left unit law for Options
// Some(x) flatMap f == f(x)
Some('a') flatMap (x => Some(fUpperChar(x)))
Some(fUpperChar('a'))
// Right unit law for Options
Some('a') flatMap (x => Some(x))
Some('a')
// Associative law for Options
Some('a') flatMap (x => Some(fUpperChar(x))) flatMap (x => Some(gCharCode(x)))
Some('a') flatMap (x => Some(fUpperChar(x)) flatMap (x => Some(gCharCode(x))))

// Try
abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch {
      case NonFatal(ex) => Failure(ex)
    }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}
case class Success[T](x: T) extends Try[T]
case class Failure(ex: Throwable) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] = // call-by-name
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}

val tryMonadTest = Try(listMonad map fUpperChar)
// the associative law - OK
(tryMonadTest flatMap (chList => Try(chList map fUpperChar)))
  .flatMap(chList => Try(chList map gCharCode))
tryMonadTest
  .flatMap (chList => Try(chList map fUpperChar) flatMap(chList => Try(chList map gCharCode)))
// the left unit law does NOT hold
val tryX = listMonad map fUpperChar
Try(tryX) flatMap (chList => Try(chList map fUpperChar))
tryX map fUpperChar // f(x), also x-expression might throw an exception whereas Try(x) can't
// the right unit law - OK
tryMonadTest flatMap (x => Try(x)) // the tryMonadTest itself
