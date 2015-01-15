import java.util.concurrent.{Executors, ExecutorService}

import parallelism._

val ints = IndexedSeq(1,6,5,7,7,5)
val l = List(1,6,5,7,7,5)

val pool: ExecutorService = Executors.newFixedThreadPool(1)
//Examples.sum(ints)(pool)//.get()

Par.parMap(l)(a => a * 2)