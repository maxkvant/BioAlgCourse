import scala.io.StdIn

object Alignments {
  val match_score = 1
  val gap_penalty = 1
  val inf: Int = 1e9.toInt

  def simpleAlignment(str1: String, str2: String): (String, String) = {
    val n = str1.length
    val m = str2.length

    val a = "\n" + str1
    val b = "\n" + str2
    val dp: Array[Array[Int]] = Array.fill[Int](n + 1, m + 1)(-inf)
    val res1: Array[Array[List[Char]]] = Array.fill[List[Char]](n + 1, m + 1)(Nil)
    val res2: Array[Array[List[Char]]] = Array.fill[List[Char]](n + 1, m + 1)(Nil)

    dp(0)(0) = 0

    for (i: Int <- 0 to n; j: Int <- 0 to m; if (i, j) != (0, 0)) {
      def get(i1 :Int, j1: Int): Option[Int] = dp.lift(i1).flatMap(_.lift(j1))

      def relax(i1: Int, char1: Char)(j1: Int, char2: Char)(add: Int): Unit = {
        get(i1, j1).foreach({ dp_val =>
          if (dp_val + add >= dp(i)(j)) {
            dp(i)(j) = dp_val + add
            res1(i)(j) = char1 :: res1(i1)(j1)
            res2(i)(j) = char2 :: res2(i1)(j1)
          }
        })
      }

      relax(i - 1, a(i))(j, '-')(-gap_penalty)
      relax(i, '-')(j - 1, b(j))(-gap_penalty)
      if (a(i) == b(j)) relax(i - 1, a(i))(j - 1, b(j))(match_score)
    }
    (res1(n)(m).mkString.reverse, res2(n)(m).mkString.reverse)
  }
}
