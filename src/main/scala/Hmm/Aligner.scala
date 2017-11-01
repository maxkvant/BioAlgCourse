package Hmm

import Hmm.Main.Matrix
import Hmm.Main.printMatrix

case class Aligner(markovModelMatch: Matrix[Double], markovModelMismatch: Matrix[Double], initProbability: Array[Double]) {
  type Array3D[T] = Array[Array[Array[T]]]

  require(markovModelMatch.length == 3)
  markovModelMatch.foreach(row => require(row.length == 3))

  require(markovModelMismatch.length == 3)
  markovModelMismatch.foreach(row => require(row.length == 3))

  import Aligner._

  def cost(i: Int, j: Int)(moveI1: Int)(moveI2: Int)(implicit strings: (String, String)): Double = {
    val (s, t) = strings
    val markovModel = (s.lift(i - 1), t.lift(j - 1)) match {
      case (Some(c1), Some(c2)) => if (c1 == c2) markovModelMatch else markovModelMismatch
      case _ => markovModelMismatch
    }
      val (di, dj) = moves(moveI2)
    val (i1, j1) = (i - di, j - dj)
    val res = markovModel(moveI1)(moveI2)
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
            (di, dj) = moves(moveI2)
            (i1, j1) = (i2 - di, j2 - dj)
            dpValue: Double = if (i1 >= 0 && j1 >= 0) dp(i1)(j1)(moveI1) else 0
          } yield (cost(i2, j2)(moveI1)(moveI2) * dpValue, i1, j1, moveI1))
            .max

        if (i1 >= 0 && j1 >= 0) {
          dp(i2)(j2)(moveI2) = dpMax
          prev1(i2)(j2)(moveI2) = (if (moves(moveI2)._1 == 1) str1(i2 - 1) else '-') :: prev1(i1)(j1)(moveI1)
          prev2(i2)(j2)(moveI2) = (if (moves(moveI2)._2 == 1) str2(j2 - 1) else '-') :: prev2(i1)(j1)(moveI1)
        }
      }
    }
    val (dpMax, maxI) = (for (i <- dp(n)(m).indices) yield (dp(n)(m)(i), i)).max
    (prev1(n)(m)(maxI).reverse.mkString, prev2(n)(m)(maxI).reverse.mkString)
  }

  def FB(str1: String, str2: String): Array3D[Double] = {
    implicit val strings = (str1, str2)
    val n = str1.length
    val m = str2.length

    val dpBefore: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpBefore(0)(0) = initProbability.clone

    for (i2 <- 0 to n;
         j2 <- 0 to m
         if (i2, j2) != (0, 0);
         moveI2 <- moves.indices) {
          dpBefore(i2)(j2)(moveI2) = (for {
              moveI1 <- moves.indices
              (di, dj) = moves(moveI2)
              (i1, j1) = (i2 - di, j2 - dj)
              if i1 >= 0 && j1 >= 0
            } yield dpBefore(i1)(j1)(moveI1) * cost(i2, j2)(moveI1)(moveI2))
              .sum
    }

    val dpAfter: Array3D[Double] = Array.fill(n + 1, m + 1, 3)(0.0)
    dpAfter(n)(m) = Array(1.0, 1.0, 1.0)
    println()

    for (i1 <- n to 0 by -1;
         j1 <- m to 0 by -1
         if (i1, j1) != (n, m);
         moveI1 <- moves.indices) {
          dpAfter(i1)(j1)(moveI1) = (for {
              moveI2 <- moves.indices
              (di, dj) = moves(moveI2)
              (i2, j2) = (i1 + di, j1 + dj)
              if i2 <= n && j2 <= m
            } yield cost(i2, j2)(moveI1)(moveI2) * dpAfter(i2)(j2)(moveI2))
              .sum
    }



    val dp = Array.fill(n + 1, m + 1, 3)(0.0)
    val res = Array.fill(n + 1, m + 1, 3)(0.0)

    for (i <- 0 to n; j <- 0 to m) {
      dp(i)(j) = (dpAfter(i)(j) zip dpBefore(i)(j)).map({case (a, b) => a * b})
      val sum = if (dp(i)(j).sum != 0) dp(i)(j).sum else 1.0
      res(i)(j) = dp(i)(j).map(_ / sum)
    }


    val norm1 = dp(n)(m).sum
    val resMatch = dp.map(_.map(_.apply(1) / norm1))
    printMatrix(resMatch)

    checkDp(dp)
    res
  }

  private def checkDp(dp: Array3D[Double]): Unit = {
    for (s: Int <- 0 to dp(0).length + dp.length - 2) {
      val cur1: Double = (for {
        i: Int <- 0 to s
        j = s - i
        if i < dp.length && j < dp(i).length
        b = () == print(i, j)
      } yield dp(i)(j).sum).sum
      println("|")

      val cur2 = (for {
        i: Int <- 0 to (s+1)
        j = (s+1) - i
        if i < dp.length && j < dp(i).length
        b = () == print(i, j)
      } yield dp(i)(j)(0)).sum
      println()



      println(s"$s: ${cur1+cur2}")
      println()
    }
  }
}

object Aligner {
  val moves: Array[(Int, Int)] = Array((1, 1), (1, 0), (0, 1))
}