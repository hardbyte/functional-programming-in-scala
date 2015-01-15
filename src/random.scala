
object random {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }


    def randomPair(rng: RNG): (Int, Int) = {
      val (i1, rng2) = rng.nextInt
      val (i2, _) = rng2.nextInt
      (i1, i2)
    }

    def positiveInt(rng: RNG): (Int, RNG) = {
      val (i, newrng) = rng.nextInt
      (if (i < 0) -(i + 1) else i, newrng)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, newrng) = positiveInt(rng)
      ((i.toDouble / Int.MaxValue.toDouble), newrng)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double(rng2)
      ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
      case 0 => (List(), rng)
      case _ => {
        val (x, r) = rng.nextInt
        val (xs, r2) = ints(count - 1)(r)
        (x :: xs, r2)
      }
    }

    type Rand[A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) =
      rng => {
        val (a, r) = s(rng)
        (f(a), r)
      }

    // Old ex 5
    def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

    def nonNegativeEven: Rand[Int] =
      map(positiveInt)(i => i - i % 2)

    // ex 5
    def doubleMap: Rand[Double] =
      map(int)(i => i / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    def randIntDouble: Rand[(Int, Double)] = both(int, double)

    // ex 7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    }

    def _ints(count: Int): Rand[List[Int]] = {
      sequence(List.fill(count)(int))
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      rng => {
        val (a, r) = f(rng)
        g(a)(r)
      }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(positiveInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s) { a => unit(f(a))}
    }

  }

  case class State[S,+A](run: S => (A,S))

  object State {
    type Rand[A] = State[RNG, A]
  }

}

