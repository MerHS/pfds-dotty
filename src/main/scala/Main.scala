import Inter._;

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)
    println(Inter.c.children)
  }

  def msg = "I was compiled by dotty :)"

}
