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

    val input1 = Array("900 google.mail.com", "50 yahoo.com", "1 intel.mail.com", "5 wiki.org")
//    val input2 = Array(50, 12, 32, 12, 28)
//
//    val inputMat = Array(
//      Array(0, 1, 0, 0),
//      Array(1, 1, 1, 0),
//      Array(0, 1, 0, 0),
//      Array(1, 1, 0, 0)
//    )

    val res = subdomainVisits(input1)

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

    // ----- Reserve for testing new data structure
//    val testData = new MovingAverage(3)
//    println(testData.next(1))
//    println(testData.next(3))
//    println(testData.next(3))
//    println(testData.next(5))

    }
}
