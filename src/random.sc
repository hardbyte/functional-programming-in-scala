import random.RNG._

var rng = simple(0xC0FFEE)
var (i, rng2) = rng.nextInt
randomPair(rng)
var (positiveI, rng3) = positiveInt(rng)
double(rng2)
intDouble(rng3)
doubleInt(rng3)
ints(5)(rng3)
int(rng)
positiveMax(50)(rng)
nonNegativeEven(rng)
doubleMap(rng)
both(int, int)(rng)
randIntDouble(rng)
sequence(List(int, positiveMax(5)))(rng)
_ints(10)(rng)
nonNegativeLessThan(10)(rng)
_map(int)(i => i / (Int.MaxValue.toDouble + 1))(rng)
def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
rollDie(simple(5))._1