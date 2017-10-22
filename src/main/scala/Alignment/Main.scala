package Alignment

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val str1 = StdIn.readLine()
    val str2 = StdIn.readLine()
    val (s1, s2) = AffineGap.align(str1, str2)
    println(s1)
    println(s2)
  }
}
