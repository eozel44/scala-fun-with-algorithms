package algorithms

object binarySearch {

  def main(args: Array[String]): Unit = {

  }

  /***
   * Binary Search
   */
  def binarySearch(l:List[Int],searchValue:Int) : Int ={
    import scala.annotation.tailrec
    @tailrec
    def recurse(arr:List[Int],search:Int,left:Int,right:Int):Int={

      if(left>right)
        return -1

      var mid = left + (right-left) / 2
      arr match {
        case arr:List[Int] if arr(mid)==search => mid
        case arr:List[Int] if arr(mid)>search  => recurse(arr,search,left,mid-1)
        case arr:List[Int] if arr(mid)<search  => recurse(arr,search,mid+1,right)
      }

    }
    recurse(l,searchValue,0,l.size-1)

  }

  /***
   * Math.sqrt implementation with binary search
   *
   */

  def binarySqrt(number:Double,epsilon:Double):Double={

    import scala.annotation.tailrec
    @tailrec
    def recurse(number:Double,left:Double,right:Double,epsilon:Double):Double ={
      var mid = left + (right-left) / 2
      mid match {
        case mid if (mid*mid+epsilon) == number => mid
        case mid if (mid*mid+epsilon) > number => recurse(number,left,mid,epsilon)
        case mid if (mid*mid+epsilon) < number => recurse(number,mid,right,epsilon)
      }
    }

    recurse(number,0d,number,epsilon)

  }


}
