package Trees

object Main {
  def main(args: Array[String]): Unit = {
    val d: Array[Array[Double]] = Array(
      Array(0, 16, 16, 10),
      Array(16, 0, 8, 8),
      Array(16, 8, 0, 4),
      Array(10, 8, 4, 0)
    )
    val labels = Array("a", "b", "c", "d")
    println(WPGMA.build(d, labels).newick)
    println(UPGMA.build(d, labels).newick)
    println(NJ.build(d, labels).newick)
    println(NJ.build(d, labels).newick)
    println(NJ.build(d, labels).newick)
    println(NJ.build(d, labels).newick)

    val d2: Array[Array[Double]] = Array(
      Array(0, 8, 8, 8, 8),
      Array(8, 0, 6, 6, 6),
      Array(8, 6, 0, 4, 4),
      Array(8, 6, 4, 0, 2),
      Array(8, 6, 4, 2, 0)
    )
    val labels2 = Array("K", "L", "M", "N", "O")
  }
}
