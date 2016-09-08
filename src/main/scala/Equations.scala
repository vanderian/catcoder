import scala.collection.immutable.HashMap
import scala.collection.immutable.Stream.Empty
import scala.math.BigDecimal.int2bigDecimal

/**
  * Created by vander on 9/8/16.
  */
object Equations extends App {
  val move = HashMap(
    (0, List(6, 9)),
    (1, List()),
    (2, List(3)),
    (3, List(2, 5)),
    (4, List()),
    (5, List(3)),
    (6, List(0, 9)),
    (7, List()),
    (8, List()),
    (9, List(0, 6))
  )

  val add = HashMap(
    (0, List(8)),
    (1, List(7)),
    (2, List()),
    (3, List(9)),
    (4, List()),
    (5, List(6, 9)),
    (6, List(8)),
    (7, List()),
    (8, List()),
    (9, List(8))
  )

  val remove = HashMap(
    (0, List()),
    (1, List()),
    (2, List()),
    (3, List()),
    (4, List()),
    (5, List()),
    (6, List(5)),
    (7, List(1)),
    (8, List(0, 6, 9)),
    (9, List(3, 5))
  )

  abstract class Tree

  abstract class OpTree extends Tree {
    val l: Tree
    val r: Tree
  }

  case class Err(string: String) extends Tree

  case class Sum(l: Tree, r: Tree, op: Int = 0) extends OpTree {
    override def toString = "(" + l + " + " + r + ")"
  }

  case class Minus(l: Tree, r: Tree, op: Int = 0) extends OpTree {
    override def toString = "(" + l + " - " + r + ")"
  }

  case class Times(l: Tree, r: Tree) extends OpTree {
    override def toString = "(" + l + " * " + r + ")"
  }

  case class Divide(l: Tree, r: Tree) extends OpTree {
    override def toString = "(" + l + " / " + r + ")"
  }

  //  case class Var(n: String) extends Tree {
  //    override def toString = n
  //  }
  case class Const(v: Int, op: Int) extends Tree {
    override def toString = s"${v.toString}($op)"
  }

  def eval(t: Tree): Int = t match {
    case Sum(l, r, o) => eval(l) + eval(r)
    case Minus(l, r, o) => eval(l) - eval(r)
    case Times(l, r) => eval(l) * eval(r)
    case Divide(l, r) => eval(l) / eval(r)
    //    case Var(n)       => env(n)
    case Const(v, o) => v
  }

  object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  def parse(eq: String): Tree = eq match {
    case Int(x) => Const(x, 0)
    case x if x.contains('+') => Sum(parse(x.takeWhile(!_.equals('+'))), parse(x.dropWhile(!_.equals('+')).drop(1)))
    case x if x.contains('-') =>
      val idx = x.lastIndexOf('-')
      Minus(parse(x.take(idx)), parse(x.drop(idx + 1)))
    case x if x.contains('*') => Times(parse(x.takeWhile(!_.equals('*'))), parse(x.dropWhile(!_.equals('*')).drop(1)))
    case x if x.contains('/') => Divide(parse(x.takeWhile(!_.equals('/'))), parse(x.dropWhile(!_.equals('/')).drop(1)))
    case x => Err(x)
  }

  def generate(tree: Tree): List[Tree] = tree match {
    case Const(v, o) =>
      val x = math.abs(v)
      val s = math.signum(v)
      Const(v, o) :: move(x).map(t => Const(t * s, 1)) ::: add(x).map(t => Const(t * s, 2)) ::: remove(x).map(t => Const(t * s, -1))
    case Sum(l, r, o) =>
      val s = for {
        x <- generate(l)
        y <- generate(r)
      } yield Sum(x, y)
      val m = for {
        x <- generate(l)
        y <- generate(r)
      } yield Minus(x, y)
      s ::: m
    case Minus(l, r, o) =>
      val m = for {
        x <- generate(l)
        y <- generate(r)
      } yield Minus(x, y)
      val s = for {
        x <- generate(l)
        y <- generate(r)
      } yield Sum(x, y)
      m ::: s
  }

  def sum(tree: Tree): (Int, Int) = tree match {
    case Const(v, o) => (o, math.abs(o))
    case x: OpTree =>
      val sl = sum(x.l)
      val sr = sum(x.r)
      (sl._1 + sr._1, sl._2 + sr._2)
  }

  def level(eq: String) = {
    val left = parse(eq.split("=")(0))
    val right = parse(eq.split("=")(1))

    val lg = generate(left)
    val rg = generate(right)

    val v = (for {
      x <- lg
      y <- rg
      if eval(x) == eval(y)
    } yield (x, y)).filter(v => {
      val sl = sum(v._1)
      val sr = sum(v._2)
      sl._1 + sr._1 == 1 && (sl._2 + sr._2 == 1 || sl._2 + sr._2 == 3)
      //      true
    })

    println(s"${v.mkString(" ")}")
  }

  def level2(eq: String) = {
    val s = eq.split("=")
    val l = generate(parse(s(0)))
    val r = generate(parse(s(1)))
    val v = for {
      x <- l
      y <- r
      if eval(x) == eval(y)
    } yield (x, y)
    println(s"${v.mkString(" ")}")
  }

  def level5(eq: String) = {
    level(eq)
    eq.zipWithIndex.filter(_._1 == '-').filter(i => eq.charAt(i._2 + 1) != '=' && eq.charAt(i._2 - 1) != '=')
    .foreach(sign => {
      val r = eq.replace('=', '-')
      val s = r.substring(0, sign._2) + '=' + r.substring(sign._2 + 1)
      level(s)
    })
  }

  println("level1")
  level("3=2")
  level("5=3")
  level("6=0")
  level("0=9")

  println("level2")
  level("0+3=9")
  level("8=2+9")
  level("1+5=0")

  println("level3")
  level("8+3=9")
  level("8-6=-8")
  level("1=2+6")

  println("level4")
  level("9+2-5+7=8-2")
  level("9+2-3-1=8-4")

  println("level5")
  level5("8+3=-1")
  level5("3-8=5")
}
