
println("With our FP List")
val l = FPGroup.List(1, 2, 3, 5)
println(l)

println("tail")
l.tail()
println("Drop")
l.drop(2)
l.drop(50)
println("dropWhile < 3")
l.dropWhile(_ < 3)
println("dropWhile false")
l.dropWhile({_ => false})
println("With a normal list...")

val lst = List(1,2,3,5)
lst.tail
lst.drop(50)
