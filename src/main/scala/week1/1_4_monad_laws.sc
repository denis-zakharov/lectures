import scala.util.Try

// List as a monad example
val m = List(1, 2, 3)
def fmFun1(x: Int) = List(x * 2)
def fmFun2(x: Int) = List(x * x)

/*-------------------------------------------------------------------*/
// Associative flatMap
m flatMap fmFun1 flatMap fmFun2
m flatMap (x => (fmFun1(x) flatMap fmFun2))

// Left unit
val x = 3
List(x) flatMap fmFun1
fmFun1(x)

// Right unit
m flatMap (x => List.apply(x))
m
/*-------------------------------------------------------------------*/



for {
  x <- Try(fmFun1(2))
  y <- Try(2/0) // below replaced with fmFun1(3)
} yield (x: Int, y: Int) => x*2 + y*3

Try(fmFun1(2)).flatMap(x => Try(fmFun1(3)).map(y => (x: Int, y: Int) => x * 2 + y * 3))
