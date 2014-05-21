package helloword
/**
 * Created by Brian on 5/14/2014.
 */

object HelloWorld {

  def sayHello(name: String) = s"Hello, $name!"

  def main(args: Array[String]) {
    println("Hello world!")
    println(sayHello("Brian"))
  }

}
