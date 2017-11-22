package DebruijnGraph

object Main {
  def main(args: Array[String]): Unit = {
    val graph = new DebruijnGraph(11)
    println(graph.cycle)
    println(graph.str)
    println(graph.str.length)
  }
}
