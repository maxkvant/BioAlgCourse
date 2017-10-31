package Hmm

import Hmm.Main.Matrix
import Hmm.Main.printMatrix

case class Aligner(markovModelMatch: Matrix[Double], markovModelMismatch: Matrix[Double], initProbability: Array[Double]) {
  type Array3D[T] = Array[Array[Array[T]]]

  require(markovModelMatch.length == 3)
  markovModelMatch.foreach(row => require(row.length == 3))

  require(markovModelMismatch.length == 3)
  markovModelMismatch.foreach(row => require(row.length == 3))

  private val moves: Array[(Int, Int)] = Array((1, 1), (1, 0), (0, 1))

  def calculateCost(i: Int, j: Int)(moveI1: Int)(moveI2: Int)(implicit strings: (String, String)): Double = {
    val (s, t) = strings
    val markovModel = (s.lift(i - 1), t.lift(j - 1)) match {
      case (Some(c1), Some(c2)) => if (c1 == c2) markovModelMatch else markovModelMismatch
      case _ => markovModelMismatch
    }
    markovModel(moveI1)(moveI2)
  }

  def Viterbi(str1: String, str2: String): (String, String) = {
    implicit val strings = (str1, str2)
    val n = str1.length
    val m = str2.length

    val dp: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dp(0)(0) = initProbability.clone
    val prev1: Array3D[List[Char]] = Array.fill(n + 1, m + 1, 3)(Nil)
    val prev2: Array3D[List[Char]] = Array.fill(n + 1, m + 1, 3)(Nil)

    for (i2 <- 0 to n; j2 <- 0 to m; if (i2, j2) != (0, 0)) {
      for (moveI2 <- moves.indices) {
        val (dpMax, i1, j1, moveI1) =
          (for {
            moveI1 <- moves.indices
            (i1: Int, j1: Int) = (i2 - moves(moveI2)._1, j2 - moves(moveI2)._2)
            dpValue: Double = if (i1 >= 0 && j1 >= 0) dp(i1)(j1)(moveI1) else 0
            cost: Double = calculateCost(i2, j2)(moveI1)(moveI2)
          } yield (cost * dpValue, i1, j1, moveI1)).max
        println(s"$i2 $j2 <- $i1 $j1 | $dpMax")
        if (i1 >= 0 && j1 >= 0) {
          dp(i2)(j2)(moveI2) = dpMax
          prev1(i2)(j2)(moveI2) = (if (moves(moveI2)._1 == 1) str1(i2 - 1) else '-') :: prev1(i1)(j1)(moveI1)
          prev2(i2)(j2)(moveI2) = (if (moves(moveI2)._2 == 1) str2(j2 - 1) else '-') :: prev2(i1)(j1)(moveI1)
        }
      }
    }
    val (dpMax, maxI) = (for (i <- dp(n)(m).indices) yield (dp(n)(m)(i), i)).max
    for (l <- 0 until 3) {
      printMatrix(dp.map(_.map(_.apply(l))))
    }
    (prev1(n)(m)(maxI).reverse.mkString, prev2(n)(m)(maxI).reverse.mkString)
  }

  def FB(str1: String, str2: String): Array3D[Double] = {
    implicit val strings = (str1, str2)
    val n = str1.length
    val m = str2.length

    val dpBefore: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpBefore(0)(0) = initProbability.clone
    println(dpBefore(0)(0).toList)

    for (i2 <- 0 to n;
         j2 <- 0 to m
         if (i2, j2) != (0, 0);
         moveI2 <- moves.indices) {
          dpBefore(i2)(j2)(moveI2) = (for {
              moveI1 <- moves.indices
              (di, dj) = moves(moveI2)
              (i1, j1) = (i2 - di, j2 - dj)
              if i1 >= 0 && j1 >= 0
            } yield dpBefore(i1)(j1)(moveI1) * calculateCost(i2, j2)(moveI1)(moveI2))
              .sum
    }

    val dpAfter: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpAfter(n)(m) = Array(1.0, 1.0, 1.0)

    for (i1 <- n to 0 by -1;
         j1 <- m to 0 by -1
         if (i1, j1) != (n, m);
         moveI1 <- moves.indices) {
          dpAfter(i1)(j1)(moveI1) = (for {
              moveI2 <- moves.indices
              (di, dj) = moves(moveI2)
              (i2, j2) = (i1 + di, j1 + dj)
              if i2 <= n && j2 <= m
            } yield calculateCost(i2, j2)(moveI1)(moveI2) * dpAfter(i2)(j2)(moveI2))
              .sum
    }

    for (l <- 0 until 3) {
      printMatrix(dpBefore.map(_.map(_.apply(l))))
    }

    println("-" * 50)

    for (l <- 0 until 3) {
      printMatrix(dpAfter.map(_.map(_.apply(l))))
    }

    println("-" * 50)

    val sumP: Double = dpBefore(n)(m).sum

    val res = Array.fill(n + 1, m + 1, 3)(0.0)
    for (i <- 0 to n; j <- 0 to m) {
      res(i)(j) = (dpAfter(i)(j) zip dpBefore(i)(j)).map(p => p._1 * p._2)
      val sum = { x: Double => if (x < 1e-4) x else 1 } apply res(i)(j).sum
      res(i)(j) = res(i)(j).map(_ / sum)
    }
    res
  }
}
