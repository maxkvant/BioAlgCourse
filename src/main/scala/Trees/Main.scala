package Trees

import Trees.TreeBuilder._

import scala.io.StdIn._

object Main {
  val treeBuilder: TreeBuilder = NJ

  def main(args: Array[String]): Unit = {
    test2()
  }

  def consoleTask(): Unit = {
    val n = readInt()
    val labels: Array[String] = readLine().split(" ")
    val read_dist: Matrix =
      for (i <- (0 until n).toArray) yield readLine().split(" ").map(_.toDouble)

    val dist = toMatrix(read_dist)

    val tree = treeBuilder.build(dist, labels)
    println(tree.newick)
  }

  def test() {
    val d: Matrix = toMatrix(Array(
      Array(0),
      Array(16, 0),
      Array(16, 8, 0),
      Array(10, 8, 4, 0)
    ))
    val labels = Array("a", "b", "c", "d")
    println(WPGMA.build(d, labels).newick)
    println(UPGMA.build(d, labels).newick)
    println(NJ.build(d, labels).newick)
  }

  def test2() {
    val labels = Array("a", "b", "c", "d", "e", "f")
    val d: Matrix = toMatrix(Array(
      Array(0),
      Array(5, 0),
      Array(4, 7, 0),
      Array(7, 10, 7, 0),
      Array(6, 9, 6, 5, 0),
      Array(8, 11, 8, 9, 8, 0)
    ))

    println(WPGMA.build(d, labels).newick)
    println(UPGMA.build(d, labels).newick)
    println(NJ.build(d, labels).newick)
  }

}
