package Trees

trait TreeBuilder {
  def build(dist: Array[Array[Double]], labels: Array[String]): Tree

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