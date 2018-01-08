val n = 10

def isPrime(num: Int): Boolean = !(2 until num).exists(x => num % x == 0 && x < num)
isPrime(6) // false
isPrime(7) // true

// get pairs from a range which sum is a prime number
(1 until n) flatMap (i =>
  (1 until i) filter (j => isPrime(i + j)) map
    (j => (i, j)))
// same, but more clear to understand
for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

val list = List(1, 2, 3)
// simple for
for (x <- list) yield x * 2
list.map(x => x * 2)

// complex for
for (x <- list if isPrime(x); i <- list) yield x + i
for (x <- list.withFilter(e => isPrime(e)); i <- list) yield x + i
list.withFilter(e => isPrime(e)) flatMap (primeNum => list map (l => l + primeNum))

// intermediate translation of the first expression above
(1 until n).flatMap(i =>
  for (y <- 1 until i if isPrime(i + y)) yield (i, y))

abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

val data = List(JObj(Map(
  "firstName" -> JStr("John"),
  "lastName" -> JStr("Smith"),
  "address" -> JObj(Map(
    "streetAddress" -> JStr("21 2nd Street"),
    "state" -> JStr("NY"),
    "postalCode" -> JNum(10021)
  )),
  "phoneNumbers" -> JSeq(List(
    JObj(Map(
      "type" -> JStr("home"), "number" -> JStr("212 555-1234")
    )),
    JObj(Map(
      "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
    )))))),
  JObj(Map(
    "firstName" -> JStr("Ivan"),
    "lastName" -> JStr("Ivanov"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("Red Square"),
      "state" -> JStr("MSK"),
      "postalCode" -> JNum(11121)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("213 666-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
      ))))))
)

//LHS of a generator as a pattern; query persons with a phone number startsWith 212
for {
  JObj(bindings) <- data
  JSeq(phones) = bindings("phoneNumbers")
  JObj(phone) <- phones
  JStr(digits) = phone("number")
  if digits startsWith "212"
} yield (bindings("firstName"), bindings("lastName"))
