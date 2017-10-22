package Trees

import scala.collection.mutable
import scala.util.Random

object NJ extends TreeBuilder {
  override def build(dist: Array[Array[Double]], labels: Array[String]): Tree = {
    checkSizes(dist, labels)

    val d: Array[Array[Double]] = dist.map(_.clone)
    val roots: mutable.Buffer[Int] = labels.indices.toBuffer[Int]
    val trees: Array[RootedTree] = labels.map(name => RootedTree(Seq(), name))

    while (roots.length > 2) {
      val n = roots.length
      val distSum: Array[Double] = labels.indices.map(i => (for (j <- roots) yield d(i)(j)).sum).toArray

      val (minD, mI, mJ, i, j) = (for {
        i <- roots
        j <- roots
        mI = distSum(i) / (n - 2)
        mJ = distSum(j) / (n - 2)
        if i < j
      } yield (d(i)(j) - mI - mJ, mI, mJ, i, j)).min

      val v: Int = i
      roots -= i
      roots -= j

      for (t <- roots) {
        val newD = 0.5 * (d(i)(t) + d(j)(t) - d(i)(j))
        d(t)(v) = newD
        d(v)(t) = newD
      }

      trees(v) = RootedTree(Seq(Edge(trees(i), 0.5 * minD + mI), Edge(trees(j), 0.5 * minD + mJ)), "")
      roots += v
    }

    if (roots.length == 1) {
      trees(roots.head)
    } else {
      val finalD: Double = d(roots(0))(roots(1))
      val trees2 = Random.shuffle(roots.map(trees))
      val rootSeq: Seq[Edge] = trees2(0).seq :+ Edge(trees2(1), finalD)
      RootedTree(rootSeq, trees2(0).name)
    }
  }
}
