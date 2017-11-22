package DebrujinGraph

object Main {
  def main(args: Array[String]): Unit = {
    val graph = new DebrujinGraph(3)
    println(graph.str)
    graph.toDot()
  }
}
