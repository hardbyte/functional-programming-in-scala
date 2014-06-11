

/** Documentation comment. */
object chapter2 {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  val abs2 = (n: Int) => if (n < 0) -n else n: Int

  //def abs3 = ?


  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }


  def fib(n: Int): Int = n match {
    /**
     * Naive implementation
     *
     * fib(0) = 0
     * fib(1) = 1
     * fib(n) = fib(n-1) + fib(n-2)
     **/
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }

  // tail recursion
  def fib2(n: Int): Int = {

    def go(n: Int, nxt: Int, res: Int): Int = n match {
      case 0 => res
      case _ => go(n-1, nxt+res, nxt)
    }

    go(n, 1, 0)
  }

  /**
   * @todo With memoization?
   * @param n number to fib
   * @return the fibbed result
   */
  //def fib3(n: Int): Int = {}


  def isSorted[A](a: Array[A], cmp: (A, A) => Boolean): Boolean = a.length match{
    case 0 => true
    case 1 => true
    case _ => cmp(a(0), a(1)) && isSorted(a.tail, cmp)

  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {

    def go(b: B): C = {
      f(a, b)
    }
    go
  }

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }

  def mycurry[A,B,C](f: (A, B) => C): A => (B => C) = {
    def g(a: A) = {
      partial1(a, f)
    }
    g
  }

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def mycompose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))


  def testPartial(): Unit = {

    var as = Array(0, 3, 4, 5)
    val cmp1: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

    println(
      isSorted(as, cmp1)
    )


    def add(a: Int, b: Int): Int= {
      a + b
    }

    println(
      partial1(1, add)(2)
    )
    println(
      partial1(1, cmp1)(2)
    )

    println(
      partial1(as, isSorted[Int])(cmp1)
    )
    println(
      partial2(as, isSorted[Int])(cmp1)
    )

  }

  def main(args: Array[String]): Unit = {
    //println(fib(6))
    //println(fib(23))

    //println(fib2(8))

    // Take argument from command line:
    //println(fib(args(0).toInt))

    def add2(a: Int, b: Int): Int = {
      a + b
    }

    def add3(a: Int, b: Int, c: Int): Int = {
      a + b + c
    }

    println(add2(3, 4))
    val cadd2 = curry(add2)

    println(cadd2(3)(4))

    // Now this doesn't work:
    //println(cadd2(3,4))

    val ucadd2 = uncurry(cadd2)
    println(ucadd2(3,4))

    // We can partially apply an argument with `_`:
    val addConst2 = add2(2, _:Int)
    println(addConst2(2))

    println(add3(3, 4, 1), 8)

    val fpAdder = add3 _
    println(fpAdder(1,1,0))


    println(mycompose(factorial, abs)(-8))

    // Using scala's built in compose ???
//    val composedFunction = factorial compose abs
//    println(
//      composedFunction(-15)
//    )

    val f = (x: Double) => math.Pi / 2 - x
    val cos = f andThen math.sin
    val cos2 = f compose math.sin
    println(cos2(0.5))

  }

}
