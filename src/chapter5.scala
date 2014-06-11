
// Streams and Lazyness

object chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    // Ex 1
    def toList: List[A] = uncons match {
      case Some((h, t)) => h ::  t.toList
      case _ => List()
    }

    // Ex 2
    def take(n: Int): Stream[A] = {
      if (n > 0) {
        uncons match {
          case Some((h, t)) => Stream.cons(h, t.take(n-1))
          case _ => Stream.empty
        }
      }
      else Stream()
    }

    // Ex 3
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case Some((h, t)) => {
        if (p(h)) Stream.cons(h, t.takeWhile(p)) else Stream.empty
      }
      case _ => Stream()
    }

    /**
     *
     * @param z The identity to start with at the right most element of the stream
     * @param f The function to apply to each element along with the result of the
     *          last application of the function (or z in the first case).
     *
     *          Non-strict evaluation of its second parameter. It can choose not to evaluate it and terminate
     *          the fold early.
     * @tparam B
     * @return
     */
    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case _ => z
    }

    // Less lazy version...
    // Also suffers due to Scala's type inference
    def foldRight2[B](z: B, f: (A, B) => B): B = uncons match {
      case Some((h, t)) => f(h, t.foldRight2(z, f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)
      //foldRight2[Boolean](false, (a, b) => p(a) || b)


    // Ex 4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // Ex 5
    def takeWhileFold(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) =>
        if(p(a))
          Stream.cons(a, b)
        else
          Stream()
      )
    }

    // Ex 6

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => {
        if (f(a))
          Stream.cons(a, b)
        else
          b
      })
    }

    def append[B>:A](newItem: B): Stream[B] =
      foldRight(Stream(newItem))((a, b) => Stream.cons(a, b))

    def extend[B>:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => Stream.cons(h, t))

    /*
      Given a function f which maps A => Stream[B] return a single Stream[B]
     */
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => f(a) extend b)




  }


  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if(as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

  // Ex 7
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Apparently this can be done better by referencing a lazy object
  // but it doesn't work for me...
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Stream.cons(a, tail)
    tail
  }

  // Ex 8
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // Ex 9
  def fibs(): Stream[Int] = {
    def next2(a: Int, b: Int): Stream[Int] = Stream.cons(a, next2(b, a + b))
   next2(0, 1)
  }

  // Ex 10
  // initial state, function to produce next state and value
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def next(s: S): Stream[A] = f(s) match {
      case Some((a, newState)) => Stream.cons(a, next(newState))
      case _ => Stream.empty[A]
    }


    next(z)
  }

  // Ex 11
  def fibs2(): Stream[Int] = unfold((0, 1)) {case (a, b) => Some((a, (b, a+b)))}

  def from2(n: Int): Stream[Int] = unfold(n) {n => Some((n, n+1))}

  def constant3[A](a: A) = unfold(a)(_ => Some((a, a)))

  val ones2 = constant3(1)

  // Ex 12
  ???

}
