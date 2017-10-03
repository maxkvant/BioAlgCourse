import scala.io.StdIn

object Main {
  import Alignments._

  def main(args: Array[String]): Unit = {
    val str1 = StdIn.readLine()
    val str2 = StdIn.readLine()
    val (s1, s2) = simpleAlignment(str1, str2)
    println(s1)
    println(s2)
  }

  println(localAlignment("abc", "bbbbbabcbbbbbbb"))
}
