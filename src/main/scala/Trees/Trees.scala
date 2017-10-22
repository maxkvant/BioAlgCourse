package Trees

sealed trait Tree {
  def newick: String
}

case class Leaf(name: String) extends Tree {
  override def newick: String = name
}

case class Edge(tree: Tree, len: Double) {
  def newickPart: String = tree.newick + ":" + len
}

case class TreeNode(edgeL: Edge, edgeR: Edge) extends Tree {
  override def newick: String = s"(${edgeL.newickPart},${edgeR.newickPart})"
}

case class RootedTree(seq: Seq[Edge], name: String) extends Tree {
  override def newick: String =
    if (seq.nonEmpty) {
      seq.map(_.newickPart).mkString("(", ",", ")") + name
    } else {
      name
    }
}

object Conversions {
  implicit def leaf(name: String): Leaf = Leaf(name)
}