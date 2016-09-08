import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by vander on 9/6/16.
  */
object Harvester extends App {
  val data = "3 4 1 4 S Z\n2 5 2 1 N S\n5 2 5 2 N Z\n23 12 23 1 N Z".split('\n').map(l => new Table(l))

  println(data.mkString(" "))
  println(data.map(t => {
    if (t.mode == "S") {
      if (t.dir == "S" || t.dir == "N") serpCol(t)
      else serpRow(t)
    }
    else {
      if (t.dir == "S" || t.dir == "N") circCol(t)
      else circRow(t)
    }
  }).mkString("\n"))

  def circRow(t: Table): String = {
    val s = t.r
    var e = t.rows + 1
    if (t.r == t.rows) e = 0
    var b = 1
    if (t.c == t.cols) b = 0

    range(s, e, if (e > s) 1 else -1).zipWithIndex.map(row => {
      val s = (row._1 - 1) * t.cols + 1
      val e = row._1 * t.cols
      line(s, e, 1, row._2 % 2 == b).mkString(" ")
    }).mkString(" ")
  }

  def circCol(t: Table): String = {
    val s = t.c
    var e = t.cols + 1
    if (t.c == t.cols) e = 0
    var b = 1
    if (t.r == t.rows) b = 0

    range(s, e, if (e > s) 1 else -1).zipWithIndex.map(col => {
      val s = col._1
      val e = t.cols * (t.rows - 1) + col._1
      line(s, e, t.cols, col._2 % 2 == b).mkString(" ")
    }).mkString(" ")
  }


  def range(s: Int, e: Int, inc: Int) = {
    val l: ListBuffer[Int] = ListBuffer()
    var t1 = s
    var t2 = e
    while (t1 != t2 || t2 < t1) {
      l.append(t1, t2)
      t1 += inc
      t2 -= inc
    }
    if (t1 == t2) l.append(t1)
    l.toList
  }

  def serpRow(t: Table): String = {
    val s = t.r
    var e = t.rows + 1
    if (t.r == t.rows) e = 0
    var b = 1
    if (t.c == t.cols) b = 0

    Range(s, e, if (e > s) 1 else -1).zipWithIndex.map(row => {
      val s = (row._1 - 1) * t.cols + 1
      val e = row._1 * t.cols
      line(s, e, 1, row._2 % 2 == b).mkString(" ")
    }).mkString(" ")
  }

  def serpCol(t: Table): String = {
    val s = t.c
    var e = t.cols + 1
    if (t.c == t.cols) e = 0
    var b = 1
    if (t.r == t.rows) b = 0

    Range(s, e, if (e > s) 1 else -1).zipWithIndex.map(col => {
      val s = col._1
      val e = t.cols * (t.rows - 1) + col._1
      line(s, e, t.cols, col._2 % 2 == b).mkString(" ")
    }).mkString(" ")
  }

  def line(s: Int, e: Int, step: Int, rev: Boolean) = {
    val v = Range(s, e + 1, step)
    if (rev) v.reverse
    else v
  }

  case class Table(rows: Int, cols: Int, r: Int, c: Int, dir: String, mode: String) {

    def this(l: Array[String]) = this(l(0).toInt, l(1).toInt, l(2).toInt, l(3).toInt, l(4), l(5))

    def this(string: String) = this(string.split(' '))
  }

}
