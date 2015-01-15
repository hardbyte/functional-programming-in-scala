
/** Documentation comment. */

// import from a package...
import hardbyte.arrayutil._


object chapter2 {

  /**
   * Define a function called abs which
   * is typed to take an Int (n) and return
   * an Int
   *
   * @param n the value to absolute
   */
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  // The same but defined inline
  val abs2 = (n: Int) => if (n < 0) -n else n: Int

  // A tail recursive function definition
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }


  // Pattern matching
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

  // Tail recursive version
  def fib2(n: Int): Int = {

    def go(n: Int, nxt: Int, res: Int): Int = n match {
      case 0 => res
      case _ => go(n-1, nxt+res, nxt)
    }

    go(n, 1, 0)
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


  def testCurry(): Unit = {
    println("Testing Curry")
    def add2(a: Int, b: Int): Int = {
      a + b
    }

    println("add2(3,4) = " + add2(3, 4))
    val cadd2 = curry(add2)

    println(cadd2(3)(4))

    // Note now this doesn't work:
    //println(cadd2(3,4))

    val partialAdd3 = cadd2(3)
    println("partialAdd3(4) = " + partialAdd3(4))


    val ucadd2 = uncurry(cadd2)
    println(ucadd2(3, 4))

    // We can partially apply an argument with `_`:
    val addConst2 = add2(2, _: Int)
    println(addConst2(2))
  }

  def testPartial(): Unit = {
    println("Testing partial")

    val as = Array(0, 3, 4, 5)
    val cmp1: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

    println(
      isSorted(as, cmp1)
    )


    def add(a: Int, b: Int): Int = {
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
    println("fib(6) = " + fib(6))

    // Take argument from command line:
    //println(fib(args(0).toInt))

    testCurry()
    testPartial()

    def add3(a: Int, b: Int, c: Int): Int = {
      a + b + c
    }

    println("Using the native partial method _")
    val fpAdder = add3 _
    println(fpAdder(1,1,0))

    val fpAdder2 = add3(2, _:Int, _:Int)
    println(fpAdder2(1,1))


    println("compose(fact, abs)(-8) = " + mycompose(factorial, abs)(-8))

    // Using scala's built in compose
    val composedFunction = factorial _ compose abs
    println(
      "fact _ compose abs (-8) = " +
      composedFunction(-8)
    )

    // Using scala's built in andThen
    val composedFunction2 = abs _ andThen factorial

    println(
      "abs _ andThen fact (-8) = " +
      composedFunction2(-8)
    )

    val f = (x: Double) => math.Pi / 2 - x
    val g = f andThen math.sin
    val g2 = math.sin _ compose f
    println(g(math.Pi))
    println(g2(math.Pi))
  }

}
