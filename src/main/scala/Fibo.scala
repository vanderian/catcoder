import scala.annotation.tailrec

/**
  * Created by vander on 9/5/16.
  */
object Fibo extends App {
  val data = List(6, 19, 28, 36, 38)
  print(data.map(fib).mkString(" "))

  def fib(n: Int) = {
    @tailrec def tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => tail(n-1, b, a + b)
    }

    tail(n, 0, 1)
  }
}
