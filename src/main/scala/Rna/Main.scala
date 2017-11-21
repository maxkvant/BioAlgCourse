package Rna

object Main {
  def main(args: Array[String]): Unit = {
    val s = "AAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGGGGGGGGGGCCCCCCCCCCCCCCCCCCCUUUUUUUUUUUUUUUUUUUUUUUUUUU"
    val t = "AAACAUGAGGAUUACCCAUGU"
    val res = Nussinov(t)
    println(res.length)
    res.foreach({case (i, j) => println(s"${s(i)} ${i+1} ${j+1} ${s(j)}")})
  }

}
