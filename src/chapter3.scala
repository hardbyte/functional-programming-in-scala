object FPGroup {

  //
  // Chapter 3
  //

  sealed trait List[+A] {
    def tail(): List[A] = this match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => xs
      case Nil => Nil
    }

    def drop(n: Int): List[A] = {
      def dropImplementation(n: Int, xs: List[A]): List[A] = n match {
        case 0 => xs
        case _ => dropImplementation(n-1, xs.tail())
      }
      dropImplementation(n, this)
    }

    def dropWhile(f: (A => Boolean)): List[A] = {

      def dropWhileImplementation(f: (A => Boolean), l: List[A]): List[A] = {
        l match {
          case Nil => Nil       // Deal with the empty list
          case Cons(x, xs) =>   // Deal with lists
            if (f(x))
              dropWhileImplementation(f, xs)
            else // return the list
              l
        }
      }
      dropWhileImplementation(f, this)
    }

  }


  case object Nil extends List[Nothing] {}
  case class Cons[+A](h: A, t: List[A]) extends List[A]

  // companion object
  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
  }

  // Main
  def main(args: Array[String]): Unit = {



    //println(l.setHead(11))
  }
}
