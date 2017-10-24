package Trees

import Trees.Conversions._
import Trees.TreeBuilder.Matrix

import scala.collection.mutable


trait PGMA extends TreeBuilder {

  private case class Node(tree: Tree, size: Int = 1, height: Double = 0.0)

  private def join(a: Node, b: Node, dist: Double): Node = {
    val newHeight = dist / 2
    val newTree: Tree = TreeNode(Edge(a.tree, newHeight - a.height), Edge(b.tree, newHeight - b.height))
    Node(newTree, a.size + b.size, newHeight)
  }

  override def build(dist: Matrix, labels: Array[String]): Tree = {
    checkSizes(dist, labels)
    val d: Matrix = dist.map(_.clone)

    val nodes: Array[Node] = labels.map(name => Node(name: Leaf))
    val roots: mutable.Buffer[Int] = labels.indices.toBuffer[Int]

    while (roots.length >= 2) {
      val (minD, i, j) = (for {
        i <- roots
        j <- roots
        if i < j
      } yield (d(i)(j), i, j)).min

      val v: Int = i
      roots -= i
      roots -= j
      for (t <- roots) {
        val newD: Double = newDist(d(t)(i), nodes(i).size)(d(t)(j), nodes(j).size)
        d(t)(v) = newD
        d(v)(t) = newD
      }
      roots += v
      nodes(v) = join(nodes(i), nodes(j), minD)
    }
    nodes(roots.head).tree
  }

  def newDist(dist1: Double, size1: Int)(dist2: Double, size2: Int): Double
}

object WPGMA extends PGMA {
  override def newDist(dist1: Double, size1: Int)(dist2: Double, size2: Int): Double = {
    (dist1 + dist2) / 2
  }
}

object UPGMA extends PGMA {
  override def newDist(dist1: Double, size1: Int)(dist2: Double, size2: Int): Double = {
    (dist1 * size1 + dist2 * size2) / (size1 + size2)
  }
}
