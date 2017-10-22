package Alignment

trait Aligner {
  def match_score = 1
  def gap_penalty = 1
  def mismatch_penalty = 1
  def gap_open_penalty = 0

  def local: Boolean
  def weight(c1: Char, c2: Char): Int = if (c1 == c2) match_score else -mismatch_penalty

  private val inf: Int = 1e9.toInt

  def align(str1: String, str2: String): (String, String) = {
    val defult_dp = if (local) 0 else -inf

    val n = str1.length
    val m = str2.length

    val a = "\n" + str1.toLowerCase
    val b = "\n" + str2.toLowerCase
    type Array3D[T] = Array[Array[Array[T]]]

    val (gap_closed, gap_a, gap_b) = (0, 1, 2)

    val dp: Array3D[Int] = Array(
      Array.fill[Int](n + 1, m + 1)(defult_dp),
      Array.fill[Int](n + 1, m + 1)(-inf),
      Array.fill[Int](n + 1, m + 1)(-inf)
    )

    dp(gap_closed)(0)(0) = 0

    val res1: Array3D[List[Char]] = Array.fill[List[Char]](3, n + 1, m + 1)(Nil)
    val res2: Array3D[List[Char]] = Array.fill[List[Char]](3, n + 1, m + 1)(Nil)

    var mx: (Int, (Int, Int)) = (0, (0, 0))

    for (i: Int <- 0 to n; j: Int <- 0 to m; if (i, j) != (0, 0)) {
      def relax(k1: Int)(k2: Int)(i1: Int, char1: Char)(j1: Int, char2: Char)(add: Int): Unit = {
        if (i1 >= 0 && j1 >= 0 && dp(k1)(i1)(j1) + add >= dp(k2)(i)(j)) {
          dp(k2)(i)(j) = dp(k1)(i1)(j1) + add
          res1(k2)(i)(j) = char1 :: res1(k1)(i1)(j1)
          res2(k2)(i)(j) = char2 :: res2(k1)(i1)(j1)
        }
      }

      relax(gap_closed)(gap_closed)(i - 1, a(i))(j, '-')(-gap_open_penalty - gap_penalty)
      relax(gap_closed)(gap_closed)(i, '-')(j - 1, b(j))(-gap_open_penalty - gap_penalty)
      relax(gap_closed)(gap_closed)(i - 1, a(i))(j - 1, b(j))(weight(a(i), b(j)))

      relax(gap_closed)(gap_a)(i, '-')(j - 1, b(j))(-gap_open_penalty - gap_penalty)
      relax(gap_a)(gap_a)(i, '-')(j - 1, b(j))(- gap_penalty)
      relax(gap_a)(gap_closed)(i, '-')(j - 1, b(j))(- gap_penalty)

      relax(gap_closed)(gap_b)(i - 1, a(i))(j, '-')(-gap_open_penalty - gap_penalty)
      relax(gap_b)(gap_b)(i - 1, a(i))(j, '-')(-gap_penalty)
      relax(gap_b)(gap_closed)(i - 1, a(i))(j, '-')(-gap_penalty)

      if (dp(0)(i)(j) > mx._1)
        mx = (dp(0)(i)(j), (i, j))
    }

    val (res_n, res_m) = if (local) mx._2 else (n, m)
    (res1(gap_closed)(res_n)(res_m).mkString.reverse, res2(gap_closed)(res_n)(res_m).mkString.reverse)
  }
}

trait SimpleAlignment extends Aligner {
  override def local = false
}
object SimpleAlignment extends SimpleAlignment

trait LocalAlignment extends Aligner {
  override def local = true
}
object LocalAlignment extends LocalAlignment

object WeightedAlignment extends SimpleAlignment {
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

object AffineGap extends SimpleAlignment {
  override def local = false
  override def gap_open_penalty: Int = 2
  override def mismatch_penalty: Int = 2
}


/*test
tccCAGTTATGTCAGgggacacgagcatgcagagac
aattgccgccgtcgttttcagCAGTTATGTCAGatc
*/