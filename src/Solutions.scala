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

    val input = Array(4,3,2,7,8,2,3,1)
    val res = findDuplicates(input)

    // ----- print if res is a linked data structure
//    while (res != null){
//      println(res.x)
//      res = res.next
//    }
    // ----- print simple type result
//    println(res)
    // ----- print if res is an object
//    println(res.x)
    // ----- print for res as collection
    res.foreach(println)
    // ----- print for more than one layer collection
//    res.foreach(x => {println("solution:"); x.foreach(println)})
    }
}
