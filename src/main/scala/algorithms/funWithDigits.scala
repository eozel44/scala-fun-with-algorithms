package algorithms

object funWithDigits {

  def main(args: Array[String]): Unit = {

    println(sumOfDigits(0) == 0)
    println(sumOfDigits(2) == 2)
    println(sumOfDigits(8) == 8)
    println(sumOfDigits(10) == 1)
    println(sumOfDigits(16) == 7)
    println(sumOfDigits(32) == 5)
    println(sumOfDigits(101) == 2)
    println(sumOfDigits(1950001) == 16)
  }

  /***
   * Problem:  Compute single-digit sum of digits

test cases:

assert(sumOfDigits(0) == 0)
assert(sumOfDigits(2) == 2)
assert(sumOfDigits(8) == 8)
assert(sumOfDigits(10) == 1)
assert(sumOfDigits(16) == 7)
assert(sumOfDigits(32) == 5)
   */

  def sumOfDigits(n:Int):Int={
    val mod = n % 10
    val k = n / 10

    (mod,n) match {
      case (0,0) => 0
      case _ => if(k>9) sumOfDigits(k)+mod else mod+k
    }
  }

  /***
   * Problem: Given Integer Division of "3" and "5"
   *
   */

  def divisionOf3and5(n:Int):String={
  val mod3 = n % 3
  val mod5 = n % 5
    (mod3, mod5) match {
      case (0,0) => "tic toc"
      case (0,_) => "tic"
      case (_,0) => "toc"
      case _ => "no"
    }
  }

  /***
   * Problem: Multiples of 3 and 5

 Find the sum of all positive multiples of 3 or 5 below N.
   */

  def sumMultiples(n:Int):Int={
    (1 to n).filter(x=> x % 3 ==0 || x % 5 ==0).foldLeft(0)((a,b)=>a+b)
  }

  /***
   *  1² + 2² + ... + 10² = 385. sum of square
   * (1 + 2 + ... + 10)² = 55² = 3025. square of sum
   */
  def sumOfSquare(n:Int):Long = (1 to n).map(x=>x*x).foldLeft(0)((a,b)=>a+b)
  def squareOfSums(n:Int):Double = Math.pow((1 to n).foldLeft(0)((a,b)=>a+b),2)

  /***
   * Input:
First line of the input file contains two integers, a and b.

Output:
Print the number of common factors of a and b.

The common factors of 10 and 15 are 1 and 5.
   */
def findCommonFactor(a:Int,b:Int):List[Int]={
  (1 to a).filter(k=> a % k ==0 && b % k ==0).toList

}

  /***
   * Problem:

He is given an array A containing N integers. His task is to update all elements of array to some minimum value x ,
  that is,  ;  such that sum of this new array is strictly greater than the sum of the initial array.
  Note that x should be as minimum as possible such that sum of the new array is greater than the sum of the initial array.

Input Format:
First line of input consists of an integer N denoting the number of elements in the array A.
Second line consists of N space separated integers denoting the array elements.

Output Format:
The only line of output consists of the value of x.

Sample Input
5
1 2 3 4 5

Sample Output
4

*
*/
  def findFirstValue(n:Int):Int = {
    val list = List.range(1,n+1)
    val avg = list.sum/list.size
    //val average = list.foldLeft((0.0, 1)) ((acc, i) => ((acc._1 + (i - acc._1) / acc._2), acc._2 + 1))._1
    list.filter(k=>k>avg).head
  }

}
