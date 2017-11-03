package Hmm

import scala.io.StdIn
import Aligner.moves

object Main {
  type Matrix[T] = Array[Array[T]]

  def printMatrix(matrix: Matrix[Double]): Unit = {
    matrix.foreach({lst =>
      lst.foreach(x => print(f"$x%.8f "))
      println()
    })
    println()
  }

  def main(args: Array[String]): Unit = {
    val s = "acgt"
    val t = "tgca"


    val markovModel = Array(
      Array(0.4, 0.3, 0.3),
      Array(0.4, 0.6, 0.0),
      Array(0.4, 0.0, 0.6)
    )

    val aligner = Aligner(markovModel,
                          initProbability = Array(1 / 3.0, 1 / 3.0, 1 / 3.0),
                          mismatchGenP = 0.1,
                          gapGenP = 0.3)

    val (s1, t1) = aligner.Viterbi(s, t)
    println(s1)
    println(t1)
    val resFB: Matrix[Double] = aligner.FB(s, t)
    printMatrix(resFB.map(_.drop(1)).drop(1))

    aligner.BacktrackFB(s, t)
  }

  def mainCoin(): Unit = {
    val coinHmm = new CoinHMM(0.8, 0.7)
    val s: String = StdIn.readLine() //  "ОРОРОРОООООО"
    val coins: List[Coin] = CoinHMM.toCoins(s)
    println(coinHmm.Viterbi(coins))

    println(coinHmm.FB(coins).map(x => (x * 10000).round / 10000.0))
  }
}
