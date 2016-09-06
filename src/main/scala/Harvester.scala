/**
  * Created by vander on 9/6/16.
  */
object Harvester extends App {
  val data = "3 4 1 1\n2 5 2 1\n5 2 5 2\n23 12 1 12".split('\n').map(l => new Table(l))

  println(data.mkString(" "))
  println(data.map(t => {
    val s = t.r
    var e = t.rows + 1
    if (t.r == t.rows) e = 0
    var b = 1
    if (t.c == t.cols) b = 0
    serp(s, e, t.cols, { r => math.abs(r - t.rows) % 2 == b})
  }).mkString("\n"))

  def serpRow(s: Int, e: Int, cols: Int, b: (Int) => Boolean): String = {
    Range(s, e, if (e > s) 1 else -1).map(row => {
      val s = (row - 1) * cols + 1
      val e = row * cols + 1
      line(s, e, b(row))
    }).mkString(" ")
  }

  def serpCol(s: Int, e: Int, cols: Int, b: (Int) => Boolean): String = {
    Range(s, e, if (e > s) 1 else -1).map(row => {
      val s = (row - 1) * cols + 1
      val e = row * cols + 1
      line(s, e, b(row))
    }).mkString(" ")
  }

  def line(s: Int, e: Int, rev: Boolean) = {
    val v = Range(s, e)
    if (rev) v.reverse.mkString(" ")
    else v.mkString(" ")
  }

  case class Table(rows: Int, cols: Int, r: Int, c: Int) {

    def this(l: Array[Int]) = this(l(0), l(1), l(2), l(3))

    def this(string: String) = this(string.split(' ').map(_.toInt))
  }

}
