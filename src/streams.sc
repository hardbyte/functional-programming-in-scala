
import chapter5._

val s = Stream(1,2,3,4,5)
s.toList
val s2 = Stream.cons(0, Stream.cons(2, Stream.empty))
s2.toList
s.take(2).toList
s.takeWhile(_ < 2).toList

s.foldRight(0)((a, b) => a + b)
s.foldRight2[Int](0, (a, b) => a + b)
//s2.forEach {print _}

s.exists(_ == 2)

def print_condition(x: Int): Boolean = {
  print(x)
  x < 4
}

s.forAll(print_condition)

s.takeWhile(print_condition).toList
s.takeWhileFold(print_condition).toList

s.map(_ * 10).toList
s.map(_ * 10).filter(_ < 25).toList

s.append(1111).toList

s.extend(Stream(10,20)).toList

def f(i: Int): Stream[Int] = Stream(-i, i, +i)
s.flatMap(f).toList

Stream(1,2,3,4).map(_ + 10).filter(_%2 ==0)

// Inf list
val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(4).toList
constant(1).take(4).toList
constant3(1).take(4).toList
ones2.take(10).toList
//constant2(1).take(4).toList
from(5).take(10).toList
fibs().take(10).toList
fibs2().take(10).toList
from2(10).take(4).toList