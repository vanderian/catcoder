import scala.annotation.tailrec

/**
  * Created by vander on 9/5/16.
  */
object Fractals extends App {
  /*
    tri Length=9 Iterations=1 => 36
    tri Length=243 Iterations=3
    tri Length=19683 Iterations=7
    tri Length=531441 Iterations=7
    tri Length=531441 Iterations=9

    sq Length=9 Iterations=1 => 60
    sq Length=243 Iterations=3
    sq Length=19683 Iterations=7
    sq Length=531441 Iterations=7
    sq Length=531441 Iterations=9
  */

  val data = List((9, 1), (243, 3), (19683, 7), (531441, 7), (531441, 9))
  println(data.map { case (l, i) => perimeterTri(l, i) }.mkString(" "))
  println(data.map { case (l, i) => perimeterSq(l, i) }.mkString(" "))

  def perimeterTri(l: Double, i: Double) = math.round(3.0 * math.pow(4.0 / 3.0, i) * l)
  def perimeterSq(l: Double, i: Double) = math.round(4.0 * math.pow(5.0 / 3.0, i) * l)
}
