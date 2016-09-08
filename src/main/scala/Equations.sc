import Equations._

import scala.collection.immutable.HashMap

val move = HashMap(
  (0, List(0, 6, 9)),
  (1, List(1)),
  (2, List(2, 3)),
  (3, List(3, 2, 5)),
  (4, List(4)),
  (5, List(5, 3)),
  (6, List(6, 0, 9)),
  (7, List(7)),
  (8, List(8)),
  (9, List(9, 0, 6))
)

val t1 = List(1, 2, 3)
val t2 = List(4, 5)

//parse("9+2-3-1")

val s = "10-10-2"
val i = s.lastIndexOf('-')
s.drop(i+1)
s.take(i)
//s.takeWhile(!_.equals('-'))
s.dropWhile(!_.equals('-'))
val l = parse("1+5")
val r = parse("0")


generate(l).map(eval)
generate(r).map(eval)

val v = for {
  x <- generate(l)
  y <- generate(r)
  if eval(x) == eval(y)
} yield (x, y)

v.mkString(" ")