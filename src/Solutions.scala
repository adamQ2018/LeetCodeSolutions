import ArrayAlgorithms._
import LinkedListAlgorithms._
import StringAlgorithms._
import DataStructures._
import DataStructures.Node._
import TreeAlgorithm._
import MathAlgorithm._
import DataBaseAlgorithm._

object Solutions {
  def main(args: Array[String]) {

//    val res = twoSumWithRestriction(Array(1, 0, -1, 2, -2), 0, 2)
      var input = Array( Array(1,1,0,0), Array(1,1,0,1), Array(0,0,1,0), Array(0,1,0,1))
      val res = findCircleNum(input)

//    while (res != null){
//      println(res.x)
//      res = res.next
//    }
//    nums1.foreach(println)
    println(res)
//    res.foreach(println)
//    res.foreach(x => {println("solution:"); x.foreach(println)})
    }
}
