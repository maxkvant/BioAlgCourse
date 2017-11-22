package Rna

object Main {
  def main(args: Array[String]): Unit = {
    def run(t: String) = {
      val res = Nussinov(t)
      println (res.length)
      res.foreach ( {case (i, j) => println (s"${t (i)} ${i + 1} ${j + 1} ${t (j)}")})
    }

    val s = "AAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGGGGGGGGGGCCCCCCCCCCCCCCCCCCCUUUUUUUUUUUUUUUUUUUUUUUUUUU"
    val t = "AAACAUGAGGAUUACC"

    run(t)
  }
}
