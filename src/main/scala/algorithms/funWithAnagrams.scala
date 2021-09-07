package algorithms

import scala.annotation.tailrec

object funWithAnagrams {

  /***
   * Problem: Find first anagrams in given List
   */

  def main(args: Array[String]): Unit = {
    val stringList = List("eren","nere","eern","ahmet","asya","yasa","fatih")
    println("input")
    stringList.foreach(k=>print(k+" "))

    def funWithAnagrams(list:List[String]):List[String] = {
        def isAnagram(left:String,right:String):Boolean ={left.sorted==right.sorted}

      @tailrec
        def recurse(l:List[String],acc:List[String]):List[String] = {
          l match {
            case Nil => acc
            case head::tail =>
              val rem = tail.filterNot(r=> isAnagram(head,r))
              recurse(rem,head::acc)
          }
        }
      recurse(list,List.empty)

    }

    println("")
    println("output")
    funWithAnagrams(stringList).foreach(k=>print(k+" "))

  }
}
