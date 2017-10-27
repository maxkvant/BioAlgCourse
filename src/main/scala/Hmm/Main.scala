package Hmm

object Main {
  def main(args: Array[String]): Unit = {
    val coinHmm = new CoinHMM(0.8, 0.7)
    val s = CoinHMM.toCoins("ОРОРОРОООООО")
    println(coinHmm.Viterbi(s))
    println(coinHmm.FB(s).map(x => (x * 100).round / 100.0))
  }
}
