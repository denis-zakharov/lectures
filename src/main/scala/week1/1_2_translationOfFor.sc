val iterable1 = List(1, 2, 3, 4, 5)
val iterable2 = iterable1.reverse
def expr1(x: Int) = x + 1
def expr2(x: Int, y: Int) = expr1(x) * expr1(y)
def filterPredicate(x: Int) = x % 2 == 0 // filter even numbers

// A simple for-expression
for (x <- iterable1) yield expr1(x)
iterable1 map(x => expr1(x))
val mapval = iterable1 map expr1

// A for with condition
for (x <- iterable1 if filterPredicate(x)) yield expr1(x)
for (x <- iterable1.withFilter(filterPredicate)) yield expr1(x)
iterable1.withFilter(filterPredicate) map expr1

// A chained for-expression
for {
  x <- iterable1
  y <- iterable2
} yield expr2(x, y)

iterable1.flatMap(x => for (y <- iterable2) yield expr2(x, y))
iterable1.flatMap(x => iterable2.map(y => expr2(x, y)))


// map via flatMap (see mapval above)
val mapval2 = iterable1 flatMap (x => List(expr1(x)))
val mapval3 = iterable1 flatMap ((x: Int) => expr1(x)).andThen(r => List.apply(r))

assert(mapval == mapval2)
assert(mapval == mapval3)