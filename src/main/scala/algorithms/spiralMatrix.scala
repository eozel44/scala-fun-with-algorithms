package algorithms

import scala.annotation.tailrec

object spiralMatrix {

  /***
   * Problem: Matrix in spiral format.

Input:  1    2   3   4
        5    6   7   8
        9   10  11  12
        13  14  15  16
Output: 1 2 3 4 8 12 16 15 14 13 9 5 6 7 11 10
   */

  def main(args: Array[String]): Unit = {

    val matrix = List(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9,10,11,12),List(13,14,15,16))
    matrix.foreach(println)

    @tailrec
    def spiralTraverseMatrix(l:List[List[Int]],acc:List[List[Int]]):List[List[Int]] = {
      l match {
        case  Nil => acc
        case head::tail =>
          val remaining = tail.transpose.reverse
          spiralTraverseMatrix(remaining,head::acc)
      }
    }
 val result = spiralTraverseMatrix(matrix,List.empty).reverse.flatten

    result.foreach(k=>print(k+" "))


  }
}
