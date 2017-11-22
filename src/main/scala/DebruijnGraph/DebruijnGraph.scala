package DebruijnGraph

import scala.collection.mutable.ArrayBuffer

class DebruijnGraph(n1: Int) {
  val n: Int = n1 - 1
  type V = Int
  
  val vertices: List[V] = (0 until (1 << n)).toList
  val edges: List[List[V]] = vertices.map(v => List(v >> 1, (v >> 1) + (1 << (n - 1))))
  val cycle: List[V] = {
    val degUsed: Array[V] = Array.fill[V](vertices.size)(0)
    val cycle = ArrayBuffer[V]()
    def dfs(v: V): Unit = {
      while (degUsed(v) < edges(v).size) {
        degUsed(v) += 1
        dfs(edges(v)(degUsed(v) - 1))
      }
      cycle.append(v)
    }
    dfs(vertices.head)
    val res = cycle.reverse.toList
    for ((a, b) <- res zip res.tail) {
      require(edges(a).contains(b))
    }
    res
  }

  val str: String = cycle.take(cycle.size - 1).map(_ & 1).map(_.toString).mkString

  require({
    val str2 = str + str
    for {
      i <- 0 until str.length
    } yield str2.substring(i, i + n1 + 1)
  }.toSet.size == (1 << n1))

  def VString(v: V): String = {
    val s = v.toBinaryString.reverse
    s + "0" * (n - s.length)
  }
}
