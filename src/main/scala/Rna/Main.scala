package Rna

import Hmm.Aligner
import Hmm.Aligner.Matrix
import Hmm.Main.printMatrix

object Main {
  def main(args: Array[String]): Unit = {
    val s = "AAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGGGGGGGGGGCCCCCCCCCCCCCCCCCCCUUUUUUUUUUUUUUUUUUUUUUUUUUU"
    Nussinov(s).foreach({case (i, j) => println(s"${s(i)} $i $j ${s(j)}")})
  }

}
