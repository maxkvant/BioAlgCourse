package Hmm

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val coinHmm = new CoinHMM(0.8, 0.7)
    val s: String = StdIn.readLine() //  "ОРОРОРОООООО"
    val coins: List[Coin] = CoinHMM.toCoins(s)
    println(coinHmm.Viterbi(coins))

    println(coinHmm.FB(coins).map(x => (x * 10000).round / 10000.0))
  }
}
