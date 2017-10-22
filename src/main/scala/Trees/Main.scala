package Trees

import scala.io.StdIn._

object Main {
  val treeBuilder: TreeBuilder = NJ

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val labels: Array[String] = readLine().split(" ")
    val dist: Array[Array[Double]] =
      for (i <- (0 until n).toArray) yield readLine().split(" ").map(_.toDouble)

    val tree = treeBuilder.build(dist, labels)
    println(tree.newick)
  }

  def test() {
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
  }

}
