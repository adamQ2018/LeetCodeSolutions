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

//    val input1 = Array[Int]()
//    val input2 = List("go","goal","goals","special")

//    val inputMat = Array(
//      Array(0, 1, 0),
//      Array(0, 0, 1),
//      Array(1, 1, 1),
//      Array(0, 0, 0)
//    )

//    val inputList = List(
//      Interval(1, 3),
//      Interval(4, 5),
//      Interval(2, 9),
//      Interval(15, 18)
//    )

    val res = findMissingRanges(Array(-1), -1, 0)

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
//    println(res.length)

    // ----- print for more than one layer collection
//    res.foreach(x => {println("solution:"); x.foreach(println)})

    // ----- Reserve for testing new data structure
//    val testData = new StackByQueue[Int]()
//    val testCommonLib =  scala.collection.mutable.Queue[Int]()
//    println(testData.top)
//    testData.push(1)
//    println(testData.top)
//    testData.push(2)
//    println(testData.top)
//    testData.pop()
//    println(testData.top)
    }
}
