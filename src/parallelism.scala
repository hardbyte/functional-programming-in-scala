object parallelism {

  import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

  /**
   * Purely functional parallel computation
   */
  object Par {

    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)


    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean) = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def sequence[A](l: List[Par[A]]): Par[List[A]] =
      l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

    def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] =
      fork {
        val fbs: List[Par[B]] = l.map(asyncF(f))
        sequence(fbs)
      }
  }


  object Examples {

    import Par._

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)

        Par.map2(
          Par.fork(sum(l)),
          Par.fork(sum(r)))(_ + _)
        //Par.map2(sum(l), sum(r))(_ + _)
        //    val sumL: Par[Int] = Par.unit(sum(l))
        //    val sumR: Par[Int] = Par.unit(sum(r))
        //    Par.get(sumL) + Par.get(sumR)
      }
  }


}