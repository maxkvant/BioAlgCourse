trait Aligner {
  def match_score = 1
  def gap_penalty = 1
  def mismatch_penalty = 1

  def local: Boolean
  def weight(c1: Char, c2: Char): Int = if (c1 == c2) match_score else -mismatch_penalty

  private val inf: Int = 1e9.toInt

  def align(str1: String, str2: String): (String, String) = {
    val defult_dp = if (local) 0 else -inf

    val n = str1.length
    val m = str2.length

    val a = "\n" + str1.toLowerCase
    val b = "\n" + str2.toLowerCase
    val dp: Array[Array[Int]] = Array.fill[Int](n + 1, m + 1)(defult_dp)
    val res1: Array[Array[List[Char]]] = Array.fill[List[Char]](n + 1, m + 1)(Nil)
    val res2: Array[Array[List[Char]]] = Array.fill[List[Char]](n + 1, m + 1)(Nil)

    dp(0)(0) = 0
    var mx: (Int, (Int, Int)) = (0, (0, 0))

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
      relax(i - 1, a(i))(j - 1, b(j))(weight(a(i), b(j)))

      if (dp(i)(j) > mx._1)
        mx = (dp(i)(j), (i, j))
    }

    val (res_n, res_m) = if (local) mx._2 else (n, m)
    (res1(res_n)(res_m).mkString.reverse, res2(res_n)(res_m).mkString.reverse)
  }
}

object SimpleAlignment extends Aligner {
  override def local = false
}

object LocalAlignment extends Aligner {
  override def local = true
}

object WeightedAlignment extends Aligner {
  override def local = false
  def unknown_char_penalty = 100

  override def weight(c1: Char, c2: Char): Int = {
    val charMap: Map[Char, Int] = Map('a' -> 0, 'c' -> 1, 'g' -> 2, 't' -> 3)
    val weights: Array[Array[Int]] = Array(
      Array( 1, -1, -1, -1),
      Array(-1,  1, -1, -1),
      Array(-1, -1,  1, -1),
      Array(-1, -1, -1,  1)
    )
    (for {
      a <- charMap.get(c1)
      b <- charMap.get(c2)
    } yield weights(a)(b))
    match {
      case Some(x) => x
      case None => -unknown_char_penalty
    }
  }
}

/*test
tccCAGTTATGTCAGgggacacgagcatgcagagac
aattgccgccgtcgttttcagCAGTTATGTCAGatc
*/