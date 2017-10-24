package Trees

import Trees.TreeBuilder.Matrix

trait TreeBuilder {
  def build(dist: Matrix, labels: Array[String]): Tree

  protected def checkSizes(dist: Array[Array[Double]], labels: Array[String]): Unit = {
    require(labels.length > 0)
    require(labels.length == dist.length)
    dist.foreach(subdist => require(subdist.length == dist.length))
    for (i <- labels.indices; j <- labels.indices; if i <= j) {
      require(dist(i)(j) == dist(i)(j))
      require(dist(i)(i) == 0)
    }
  }
}

object TreeBuilder {
  type Matrix = Array[Array[Double]]

  // 0         0 a b
  // a 0   =>  a 0 c ...
  // b c 0     b c 0
  //  ...       ...

  def toMatrix(d: Array[Array[Double]]): Matrix = {
    for {
      i <- d.indices.toArray
      res = for {
        j <- d.indices.toArray
        dij = if (i >= j) d(i)(j) else d(j)(i)
      } yield dij
    } yield res
  }
}