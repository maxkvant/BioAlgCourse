package Rna

object Nussinov {
  def complement(a: Char, b: Char): Boolean = Set(
    ('A', 'U'),
    ('U', 'A'),
    ('G', 'C'),
    ('C', 'G')
  ).apply(a, b)

  def apply(s: String): List[(Int, Int)] = {
    val n = s.length
    val dp = Array.fill[Double](n+1, n)(0.0)
    val prev = Array.fill[List[(Int, Int)]](n, n)(Nil)

    for (len <- 4 to n; l <- 0 to (n - len); r = l + len - 1) {
      val (dp1, lAns, rAns) = (for  {
        k <- l until r
        dp_sum = dp(l)(k) + dp(k+1)(r)
      } yield (dp_sum, prev(l)(k), prev(k+1)(r))).maxBy(_._1)

      dp(l)(r) = dp1
      prev(l)(r) = lAns ++ rAns

      if (complement(s(l), s(r)) && dp1 < dp(l + 1)(r - 1) + 1) {
        dp(l)(r) = dp(l + 1)(r - 1) + 1
        prev(l)(r) = (l, r) :: prev(l + 1)(r - 1)
      }
    }
    prev(0)(n-1)
  }
}
