package Hmm

import scala.io.StdIn

object Main {
  type Matrix[T] = Array[Array[T]]

  def printMatrix(matrix: Matrix[Double]): Unit = {
    matrix.foreach({lst =>
      lst.foreach(x => print(f"$x%.10f "))
      println()
    })
    println()
  }

  def main(args: Array[String]): Unit = {
    val s = "acgt"
    val t = "tgca"
    val markovModelMatch = Array(
      Array(0.8, 0.1, 0.1),
      Array(0.8, 0.2, 0.0),
      Array(0.8, 0.0, 0.2)
    )

    val markovModelMismatch = Array(
      Array(1 / 3.0, 1 / 3.0, 1 / 3.0),
      Array(0.4, 0.6, 0.0),
      Array(0.4, 0.0, 0.6)
    )

    val aligner = Aligner(markovModelMatch, markovModelMismatch, Array(1 / 3.0, 1 / 3.0, 1 / 3.0))
    val (s1, t1) = aligner.Viterbi(s, t)
    println(s1)
    println(t1)
    val resFB = aligner.FB(s, t)
    val tableNames = Array("match", "gap", "gap")
    for (p <- tableNames.zipWithIndex) {
      println(p._1)
      printMatrix(resFB.map(_.map(_.apply(p._2))))
    }
  }

  def mainCoin(): Unit = {
    val coinHmm = new CoinHMM(0.8, 0.7)
    val s: String = StdIn.readLine() //  "ОРОРОРОООООО"
    val coins: List[Coin] = CoinHMM.toCoins(s)
    println(coinHmm.Viterbi(coins))

    println(coinHmm.FB(coins).map(x => (x * 10000).round / 10000.0))
  }
}
