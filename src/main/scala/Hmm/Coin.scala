package Hmm

sealed trait Coin

case object Head extends Coin

case object Tail extends Coin

sealed trait State

case object Sym extends State

case object Unsym extends State

case class CoinHMM(pSymSym: Double, pUnsymUnsym: Double) {
  require(0 <= pSymSym && pSymSym <= 1)
  require(0 <= pUnsymUnsym && pUnsymUnsym <= 1)

  private val markovModel = Array(
    Array(pSymSym, 1.0 - pSymSym),
    Array(1.0 - pUnsymUnsym, pUnsymUnsym)
  )

  private val coinToP = Array(
    Array(0.5, 0.5),
    Array(0.9, 0.1)
  )

  private val toState: Array[State] = Array(Sym, Unsym)

  type Matrix[T] = Array[Array[T]]

  def Viterbi(coins: List[Coin]): List[State] = {
    val coinsInt: List[Int] = coins.map({
      case Head => 0
      case Tail => 1
    })
    val n = coins.size

    val dp: Matrix[Double] = Array.fill(n, 2)(0.0)
    dp(0) = Array(
      0.5 * coinToP(0)(coinsInt.head),
      0.5 * coinToP(1)(coinsInt.head)
    )
    val prev: Matrix[List[State]] = Array.fill(n, 2)(Nil)
    prev(0) = toState.map(x => List(x))

    for (i <- 1 until n) {
      val c: Int = coinsInt(i)
      for (j2 <- 0 to 1) {
        val (costMax, jMax) =
          (for (j1 <- 0 to 1)
            yield (dp(i - 1)(j1) * markovModel(j1)(j2) * coinToP(j2)(c), j1)
            ).max

        val state: State = toState(j2)

        dp(i)(j2) = costMax
        prev(i)(j2) = state :: prev(i - 1)(jMax)
      }
    }

    (if (dp(n - 1)(0) > dp(n - 1)(1)) prev(n - 1)(0) else prev(n - 1)(1)).reverse
  }

  def FB(coins: List[Coin]): List[Double] = {
    val coinsInt: List[Int] = coins.map({
      case Head => 0
      case Tail => 1
    })
    val n = coins.size

    val dpBefore: Matrix[Double] = Array.fill(n, 2)(0.0)
    dpBefore(0) = Array(
      0.5 * coinToP(0)(coinsInt.head),
      0.5 * coinToP(1)(coinsInt.head)
    )

    for (i <- 1 until n) {
      val c: Int = coinsInt(i)
      for (j2 <- 0 to 1) {
        dpBefore(i)(j2) =
          (for (j1 <- 0 to 1)
            yield dpBefore(i - 1)(j1) * markovModel(j1)(j2) * coinToP(j2)(c)
            ).sum
      }
    }

    val dpAfter: Matrix[Double] = Array.fill(n + 1, 2)(0.0)
    dpAfter(n - 1) = Array(1.0, 1.0)

    for (i <- (n - 2) to 0 by -1) {
      val c: Int = coinsInt(i + 1)
      for (j1 <- 0 to 1) {
        dpAfter(i)(j1) =
          (for (j2 <- 0 to 1)
            yield markovModel(j1)(j2) * coinToP(j2)(c) * dpAfter(i + 1)(j2)
            ).sum
      }
    }

    val coinsP = dpBefore(n - 1).sum

    def res(t: Int) = (for (i <- 0 until n) yield dpBefore(i)(t) * dpAfter(i)(t) / coinsP).toList
    res(0)
  }

  private def printMatrix[T](matrix: Matrix[T]): Unit = {
    matrix.foreach(x => println(x.mkString(" ")))
    println()
  }
}

object CoinHMM {
  def toCoins(s: String): List[Coin] = {
    s.map({
      case 'О' => Head
      case 'Р' => Tail
      case _ => throw new RuntimeException("unexpected char")
    }).toList
  }
}
