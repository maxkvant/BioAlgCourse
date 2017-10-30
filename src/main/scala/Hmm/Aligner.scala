package Hmm

import Hmm.Main.Matrix

case class Aligner(markovModelMatch: Matrix[Double], mismatchRow: Array[Double], initProbability: Array[Double]) {
  type Array3D[T] = Array[Array[Array[T]]]

  require(markovModelMatch.length == 3)
  markovModelMatch.foreach(row => require(row.length == 3))
  require(mismatchRow.length == 3)
  require(initProbability.length == 3)

  private val markovModelMismatch: Matrix[Double] = Array(
    mismatchRow,
    markovModelMatch(1),
    markovModelMatch(2)
  )

  private val moves: Array[(Int, Int)] = Array((1, 1), (1, 0), (0, 1))

  def calculateCost(s1: String, i: Int)(s2: String, j: Int)(moveI1: Int)(moveI2: Int): Double = {
    (s1.lift(i), s2.lift(j)) match {
      case (Some(c1), Some(c2)) =>
        val markovModel = if (s1(i) == s2(j)) markovModelMatch else markovModelMismatch
        markovModel(moveI1)(moveI2)
      case _ => 0
    }
  }

  def Viterbi(str1: String, str2: String): (String, String) = {
    val s1 = " " + str1
    val s2 = " " + str2
    val n = str1.length
    val m = str2.length

    val dp: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dp(0)(0) = initProbability
    val prev1: Array3D[List[Char]] = Array.fill(n + 1, m + 1, 3)(Nil)
    val prev2: Array3D[List[Char]] = Array.fill(n + 1, m + 1, 3)(Nil)

    for (i2 <- 0 to n; j2 <- 0 to m; if (i2, j2) != (0, 0)) {
      for (moveI2 <- moves.indices) {
        val (dpMax, i1, j1, moveI1) =
          (for {
            moveI1 <- moves.indices
            (i1: Int, j1: Int) = (i2 - moves(moveI2)._1, j2 - moves(moveI2)._2)
            dpValue: Double = if (i1 >= 0 && j1 >= 0) dp(i1)(j1)(moveI1) else 0
            cost: Double = calculateCost(s1, i2)(s2, j2)(moveI1)(moveI2)
          } yield (cost * dpValue, i1, j1, moveI1)).max
        if (i1 >= 0 && j1 >= 0) {
          dp(i2)(j2)(moveI2) = dpMax
          prev1(i2)(j2)(moveI2) = (if (moves(moveI2)._1 == 1) s1(i2) else '-') :: prev1(i1)(j1)(moveI1)
          prev2(i2)(j2)(moveI2) = (if (moves(moveI2)._2 == 1) s2(j2) else '-') :: prev2(i1)(j1)(moveI1)
        }
      }
    }
    val (dpMax, maxI) = (for (i <- dp(n)(m).indices) yield (dp(n)(m)(i), i)).max
    (prev1(n)(m)(maxI).reverse.mkString, prev2(n)(m)(maxI).reverse.mkString)
  }

  def FB(str1: String, str2: String): Array3D[Double] = {
    val s1 = " " + str1
    val s2 = " " + str2
    val n = str1.length
    val m = str2.length

    val dpBefore: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpBefore(0)(0) = initProbability

    for (i2 <- 0 to n; j2 <- 0 to m; if (i2, j2) != (0, 0)) {
      for (moveI2 <- moves.indices) {
        dpBefore(i2)(j2)(moveI2) =
          (for {
            moveI1 <- moves.indices
            (i1: Int, j1: Int) = (i2 - moves(moveI2)._1, j2 - moves(moveI2)._2)
            dpValue: Double = if (i1 >= 0 && j1 >= 0) dpBefore(i1)(j1)(moveI1) else 0
            cost: Double = calculateCost(s1, i2)(s2, j2)(moveI1)(moveI2)
          } yield cost * dpValue).sum
      }
    }

    val dpAfter: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpAfter(n)(m) = Array(1.0, 1.0, 1.0)

    for (i1 <- n to 0 by -1; j1 <- m to 0 by -1; if (i1, j1) != (n, m)) {
      for (moveI1 <- moves.indices) {
        dpAfter(i1)(j1)(moveI1) =
          (for {
            moveI2 <- moves.indices
            (i2: Int, j2: Int) = (i1 + moves(moveI2)._1, j1 + moves(moveI2)._2)
            dpValue: Double = if (i2 <= n && j2 <= m) dpAfter(i2)(j2)(moveI1) else 0
            cost: Double = calculateCost(s1, i2)(s2, j2)(moveI1)(moveI2)
          } yield cost * dpValue).sum
      }
    }

    val sumP: Double = dpBefore(n)(m).sum

    val res = Array.fill(n + 1, m + 1, 3)(0.0)
    for (i <- 0 to n; j <- 0 to m) {
      res(i)(j) = (dpAfter(i)(j) zip dpBefore(i)(j)).map(p => p._1 * p._2)
      res(i)(j) = norm(res(i)(j))
    }
    res
  }

  private def round1(x: Double): Double = (x * 1000.0).round / 1000.0

  private def norm(a: Array[Double]): Array[Double] = {
    val sum = a.sum
    a.map(x => round1(x / sum))
  }
}
