
val abs = (x:Double) => if (x > 0) x else -x
def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double = {
    if (abs(guess * guess - x) / x < 1e-9)
      guess
    else
      sqrtIter((guess + x / guess) / 2)
  }

  sqrtIter(x / 2)
}
sqrt(2)
sqrt(1e-6)
sqrt(1e-20)
sqrt(1e50)
def gcd(a: Int, b: Int): Int =
  if(b == 0) a else gcd(b, a % b)


gcd(100, 234500)