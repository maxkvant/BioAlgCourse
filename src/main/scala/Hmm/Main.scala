package Hmm

import scala.io.StdIn

object Main {
  type Matrix[T] = Array[Array[T]]

  def printMatrix[T](matrix: Matrix[T]): Unit = {
    matrix.foreach(x => println(x.mkString(" ")))
    println()
  }

  def main(args: Array[String]): Unit = {
    val s = "AGA"
    val t = "AGAGA"
    val markovModelMatch = Array(
      Array(0.8, 0.1, 0.1),
      Array(0.5, 0.5, 0.0),
      Array(0.5, 0.0, 5.0)
    )
    val mismatchRow = Array(0.1, 0.45, 0.45)
    val aligner = Aligner(markovModelMatch, mismatchRow, Array(0.3, 0.3, 0.3))
    println(aligner.Viterbi(s, t))
    val resFB = aligner.FB(s, t)
    println("match")
    printMatrix(resFB.map(_.map(_.apply(0))))
    println("gap >")
    printMatrix(resFB.map(_.map(_.apply(1))))
    println("gap v")
    printMatrix(resFB.map(_.map(_.apply(2))))
  }

  def mainCoin(): Unit = {
    val coinHmm = new CoinHMM(0.8, 0.7)
    val s: String = StdIn.readLine() //  "ОРОРОРОООООО"
    val coins: List[Coin] = CoinHMM.toCoins(s)
    println(coinHmm.Viterbi(coins))

    println(coinHmm.FB(coins).map(x => (x * 10000).round / 10000.0))
  }
}
