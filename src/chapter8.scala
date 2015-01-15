//import random.{RNG,State}
//
//
///**
// * Chapter 8 - Testing with properties
// *
// * EX 1
// *
// * Test that sum(range(1,n)) = n(n-1)/2
// * Test that sum([a]*n) == n*a
// * Test that sum([]) == 0
// *
// * EX 2 (max fo list)
// *
// * prop = forAll( [a] * n )(l.max == a) &&
// *        forAll( range(1, n) ) (l.max == n)
// *        forAll( range(1, n).reverse)( l.max == n)
// *
// *
// * Ideas:
// *
// * property chaining?
// *
// */
//object chapter8 {
//
//  object Prop {
//    type FailedCase = String
//    type SuccessCount = Int
//  }
//
//  trait Prop {
//    var successcount: Prop.SuccessCount = 0
//
//    def check: Either[(FailedCase, SuccessCount), SuccessCount]
//
//    def &&(p: Prop): Prop = {
//      this.check match {
//        case Left(f, n) => Prop.this.SuccessCount += 1
//        case Right(n) =>
//      }
//      p
//    }
//  }
//
//  case class Gen[A](sample: State[RNG, A])
//
//  object Gen {
//    def unit[A](a: => A): Gen[A] =
//      Gen(State.unit(a))
//
//    def boolean: Gen[Boolean] =
//      Gen(State(RNG.boolean))
//
//    def choose(start: Int, stopExclusive: Int): Gen[Int] =
//      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
//
//    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
//      Gen(State.sequence(List.fill(n)(g.sample)))
//  }
//
//  def listOf[A](a: Gen[A]): Gen[List[A]]
//
//  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
//
//
//}
//
//
