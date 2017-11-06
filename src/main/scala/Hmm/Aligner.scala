package Hmm

import Hmm.Aligner.Matrix
import Hmm.Main.printMatrix

object Aligner {
  val moves: Seq[(Int, Int)] = List((1, 1), (1, 0), (0, 1))
  type Matrix[T] = Array[Array[T]]
}

case class Aligner(markovModel: Matrix[Double], initProbability: Array[Double], mismatchGenP: Double, gapGenP: Double) {
  type Array3D[T] = Array[Array[Array[T]]]

  require(markovModel.length == 3)
  markovModel.foreach(row => require(row.length == 3))

  import Aligner._

  def cost(i: Int, j: Int)(moveI1: Int)(moveI2: Int)(implicit strings: (String, String)): Double = {
    val (s, t) = strings
    val (di, dj) = moves(moveI2)
    val genCost = (s.lift(i - 1), t.lift(j - 1), moveI2) match {
      case (_, _, 1) => gapGenP
      case (_, _, 2) => gapGenP
      case (Some(c1), Some(c2), 0) => if (c1 == c2) 1.0 else mismatchGenP
      case _ => 1.0
    }
    markovModel(moveI1)(moveI2) * genCost
  }

  def Viterbi(str1: String, str2: String): (String, String) = {
    implicit val strings: (String, String) = (str1, str2)
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

  def FB(str1: String, str2: String): Matrix[Double] = {
    implicit val strings: (String, String) = (str1, str2)
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

    for (i <- 0 to n; j <- 0 to m) {
      dp(i)(j) = (dpAfter(i)(j) zip dpBefore(i)(j)).map({case (a, b) => a * b})
    }

    val norm1 = dp(n)(m).sum

    val res = dp.map(_.map(_.apply(0) / norm1))
    res
  }

  def BacktrackFB(s: String, t: String): Matrix[Double] = {
    implicit val strings: (String, String) = (s, t)
    println("backtrackFB")

    val n = s.length
    val m = t.length
    val ans: Matrix[Double] = Array.fill(n, m)(0.0)
    var sum: Double = 0
    def dfs(i: Int, j: Int, moveI1: Int, p: Double, matches: List[(Int,Int)]): Unit = {
      if (i == n && j == m) {
        sum += p
        for ((i, j) <- matches) {
          ans(i)(j) += p
        }
      }
      for (((di, dj), moveI2) <- moves.zipWithIndex if i + di <= n && j + dj <= m) {
        val matches2 = if (moveI2 == 0) (i, j) :: matches else matches
        dfs(i + di, j + dj, moveI2, p * cost(i + di, j + dj)(moveI1)(moveI2), matches2)
      }
    }
    for (i <- moves.indices) {
      dfs(0, 0, i, initProbability(i), Nil)
    }
    val res = ans.map(_.map(_ / sum))
    printMatrix(res)
    res
  }
}