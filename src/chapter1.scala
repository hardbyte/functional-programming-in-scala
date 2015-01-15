/**
 * Hello World in Scala
 *
 */

object chapter1 {

  def sayHello(name: String) = s"Hello, $name!"

  def verboseGreeter(name: String): String = {
    val greeting: String = "Greetings from " + name
    greeting + "!"
  }

  def main(args: Array[String]) {
    println("Hello world!")
    println(sayHello("function call"))
    println(verboseGreeter("another function call"))

    // Anonymous inline function (+ call)
    println( ((n: String) => s"hi $n")("brian"))
  }

}
