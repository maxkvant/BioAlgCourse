package DebrujinGraph

object Main {
  def main(args: Array[String]): Unit = {
    val graph = new DebrujinGraph(5)
    println(graph.str)
    graph.cycle.zipWithIndex.foreach({ case (v, i) =>
      println(" " * i + graph.VString(v))
    })
    graph.saveDot()
  }
}
