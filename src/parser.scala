/**
 * 7/16/2014.
 *
 * Parser Combinators
 *
 * http://henkelmann.eu/2011/01/13/an_introduction_to_scala_parser_combinators
 */

object parser {


  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError,A]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
      ParserOps[String] = ParserOps(f(a))
    def char(c: Char): Parser[Char]// = string(c.toString) map (_.charAt(0))

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    def map[A,B](a: Parser[A])(f: A => B): Parser[B]

   // def many[A](p: Parser[A]): Parser[List[A]] =
    //  map(many(char('a')))(_.size)

//    def map2[A,B,C](p: Parser[A], p2: Parser[B])(
//      f: (A,B) => C): Parser[C] =
//      map(product(p, p2))(f.tupled)
//    def many1[A](p: Parser[A]): Parser[List[A]] =
//      map2(p, many(p))(_ :: _)


    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      //def many = self.many(p)

    }
  }


  object Parsers {

  }

  def main(args: Array[String]): Unit = {

    println("hi")
    val s = "abbracadabra"

  }
}
