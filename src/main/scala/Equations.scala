import scala.collection.immutable.HashMap

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

  object Op extends Enumeration {
    type Action = Value
    val Add, Remove = Value

  }

  implicit class OpExt(valueSet: Op.ValueSet) {
    def exists(op: Op.Value): Int = if (valueSet.contains(op)) 1 else 0
  }

  abstract class Tree

  abstract class OpTree extends Tree {
    val l: Tree
    val r: Tree
    val op: Op.ValueSet
  }

  case class Err(string: String) extends Tree

  case class Sum(l: Tree, r: Tree, op: Op.ValueSet = Op.ValueSet.empty) extends OpTree {
    override def toString = s"$l+$r"
  }

  case class Minus(l: Tree, r: Tree, op: Op.ValueSet = Op.ValueSet.empty) extends OpTree {
    override def toString = s"$l-$r"
  }

  //  case class Times(l: Tree, r: Tree) extends Tree {
  //    override def toString = "(" + l + " * " + r + ")"
  //  }

  //  case class Divide(l: Tree, r: Tree) extends Tree {
  //    override def toString = "(" + l + " / " + r + ")"
  //  }

  //  case class Var(n: String) extends Tree {
  //    override def toString = n
  //  }
  case class Const(v: Int, op: Op.ValueSet = Op.ValueSet.empty) extends Tree {
    override def toString = s"$v"
  }

  def eval(t: Tree): Int = t match {
    case Sum(l, r, o) => eval(l) + eval(r)
    case Minus(l, r, o) => eval(l) - eval(r)
    //    case Times(l, r) => eval(l) * eval(r)
    //    case Divide(l, r) => eval(l) / eval(r)
    //    case Var(n)       => env(n)
    case e: Err => -999
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
    case Int(x) => Const(x)
    case x if x.contains('+') => Sum(parse(x.takeWhile(!_.equals('+'))), parse(x.dropWhile(!_.equals('+')).drop(1)))
    case x if x.contains('-') =>
      val idx = x.lastIndexOf('-')
      Minus(parse(x.take(idx)), parse(x.drop(idx + 1)))
    //    case x if x.contains('*') => Times(parse(x.takeWhile(!_.equals('*'))), parse(x.dropWhile(!_.equals('*')).drop(1)))
    //    case x if x.contains('/') => Divide(parse(x.takeWhile(!_.equals('/'))), parse(x.dropWhile(!_.equals('/')).drop(1)))
    case x => Err(x)
  }

  def generate(tree: Tree): List[Tree] = tree match {
    case Const(v, o) =>
      val x = math.abs(v)
      val s = math.signum(v)
      val i = x.toString.map(_.asDigit)

      def nums(op: (Int) => List[Int]): List[Int] = i.map(x => op(x)).zipWithIndex
        .flatMap(l => l._1.map(n => s"${i.take(l._2).mkString}$n${i.drop(l._2 + 1).mkString}"))
        .map(_.toInt)
        .toList

      Const(v, o) :: nums(x => move(x))
        .map(t => Const(t * s, Op.Add + Op.Remove)) ::: nums(x => add(x))
        .map(t => Const(t * s, Op.ValueSet(Op.Add))) ::: nums(x => remove(x))
        .map(t => Const(t * s, Op.ValueSet(Op.Remove)))
    case Sum(l, r, o) =>
      val s = for {
        x <- generate(l)
        y <- generate(r)
      } yield Sum(x, y)
      val m = for {
        x <- generate(l)
        y <- generate(r)
      } yield Minus(x, y, Op.ValueSet(Op.Remove))
      s ::: m
    case Minus(l, r, o) =>
      val m = for {
        x <- generate(l)
        y <- generate(r)
      } yield Minus(x, y)
      val s = for {
        x <- generate(l)
        y <- generate(r)
      } yield Sum(x, y, Op.ValueSet(Op.Add))
      m ::: s
  }

  def sum(tree: Tree): (Int, Int) = tree match {
    case Const(v, o) => (o.exists(Op.Add), o.exists(Op.Remove))
    case x: OpTree =>
      val sl = sum(x.l)
      val sr = sum(x.r)
      (sl._1 + sr._1 + x.op.exists(Op.Add), sl._2 + sr._2 + x.op.exists(Op.Remove))
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
      sl._1 + sr._1 == 1 && sl._2 + sr._2 == 1
    }).map(x => s"${x._1}=${x._2}")

    println(v.mkString(" "))

    eq.zipWithIndex
      .filter(x => x._1 == '-' || x._1 == '=')
      .filter(_._2 > 0)
      .filter(i => !Array('=', '-').contains(eq.charAt(i._2 + 1)) && !Array('=', '-').contains(eq.charAt(i._2 - 1)))
      .filter(_._1 == '-')
      .map(sign => {
        val r = eq.replace('=', '-')
        val s = r.substring(0, sign._2) + '=' + r.substring(sign._2 + 1)
        val sp = s.split('=')
        (parse(sp(0)), parse(sp(1)))
      })
      .filter(x => eval(x._1) == eval(x._2))
      .foreach(x => println(s"${x._1}=${x._2}"))
  }

  println("level1")
  level("3=2")
  level("5=3")
  level("6=0")
  level("0=9")

  println()
  println("level2")
  level("0+3=9")
  level("8=2+9")
  level("1+5=0")

  println()
  println("level3")
  level("8+3=9")
  level("8-6=-8")
  level("1=2+6")

  println()
  println("level4")
  level("9+2-5+7=8-2")
  level("9+2-3-1=8-4")

  println()
  println("level5")
  level("8+3=-1")
  level("3-8=5")

  println()
  println("level6")
  level("9+8=14")
  level("14+9=11")
  level("98-60=-22")

  println()
  println("level7")
  level("93+27-30+16=68")
  level("78+23-89+82=94+2-18")
}
