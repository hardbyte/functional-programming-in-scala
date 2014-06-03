
object errors {

  // "normal" way in Java/Python etc of dealing with
  // exceptional circumstances
  def exceptional_mean(xs: Seq[Double]): Double = {
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list")
    else
      xs.sum / xs.length
  }

  /* Option has two cases: it can be defined, in which case it will
     be a Some, or it can be undefined, and therefore be None.
   */
  sealed trait Option[+A] {
    /*
     Last chapter we put all the functions that operated on List
     in the  companion object. Here we are going to place our
     functions, when possible, inside the body of the Option
     trait, so they can be Option called with OO syntax. This
     is a stylistic choice with no real significance, and we'll
     use both styles throughout.
     */

    // Ex 1

    // map f to Some values
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    // returns the first Option if its defined otherwise the second Option
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) =>  Some(a)
      case _ => None
    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def option_mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def option_variance(xs: Seq[Double]): Option[Double] = {
    val m = option_mean(xs)
    def calcVar(themean: Double): Option[Double] = {
      val squaredDiffs = xs map (x => math.pow(x - themean, 2))
      option_mean(squaredDiffs)
    }
    m flatMap calcVar
  }



  def main(args: Array[String]) {
    println("Running the errors code")

    println("Passing in an array")
    println(option_mean(Seq(2.02, 0.2)))

    println("Passing in empty")
    println(option_mean(Seq()))

    println("Variance...")
    println(option_variance(Seq(2.03, 4.2)))
    println(option_variance(Seq()))
  }
}

