import DataStructures._
import DataStructures.Node._
import MathAlgorithm._

object ArrayAlgorithms {

  // -------------------------- *** Problem: Two Sum *** ----------------------

  def twoSum (arr: Array[Int], target: Int): (Int, Int) = {

    val indArr: Array[Int] = arr.map( x => arr.indexOf(x) )

    val valueMap = (arr zip indArr).toMap

    var ind = 0
    val maxLen = arr.length

    var compLoc = -1

    while ( (ind <= maxLen) && compLoc == -1 ) {
      println(s"this element is ${arr(ind)}")
      val complement = target - arr(ind)
      compLoc = try valueMap(complement) catch {case e: Exception => -1}

      println(s"$compLoc")
      ind += 1
    }

    if (compLoc == -1) throw new IllegalArgumentException("No such indices matching the requirement.")

    (ind - 1, compLoc)

  }


  // -------------------------- *** Problem: Combine two sorted array *** ----------------------
  def combineSortedArray(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {

    val minArr = if (arr1.head <= arr2.head) arr1 else arr2
    val maxArr = if (arr1.head <= arr2.head) arr2 else arr1

    val check = if (maxArr.head >= minArr.last) "1" else "proceed"

    val len1 = minArr.length
    val len2 = maxArr.length

    check match {

      case "1" => (minArr ++ maxArr)

      case _ => {

        var res = Array(minArr.head)


        var ind1 = 1
        var ind2 = 0

        while (ind1 < len1){

          while (maxArr(ind2) <= minArr(ind1) && ind2 < len2 ) {
            res = res :+ maxArr(ind2)
            ind2 += 1
          }

          res = res :+ minArr(ind1)

          ind1 += 1
        }

        while (ind2 < len2){
          res = res :+ maxArr(ind2)
          ind2 += 1
        }

        res
      }
    }
  }

  // -------------------------- *** Problem: Median of two sorted array *** ----------------------
  // -------------------------- ### Incomplete ### ----------------------

  def medianOfTwoSortedArr (arr1: Array[Int], arr2: Array[Int]) ={

    val arr1LongerInd = arr1.length >= arr2.length

    val longerArr = if (arr1LongerInd) arr1 else arr2
    val shorterArr = if (arr1LongerInd) arr2 else arr1

    val minLen = shorterArr.length
    val maxLen = longerArr.length

    var iMin = 0; var iMax= minLen

    var i = (iMin + iMax)/2
    var j = (minLen + maxLen + 1)/2 - i

    var ind1 = false; var ind2 = false

    if (i <= 0 && j >= maxLen - 1) {i = 0; j = maxLen - 1; ind1 = true ; ind2 = true}
    else if (i >= minLen -1 && j <= 0) {i = minLen - 1; j = 0; ind1 = true ; ind2 = true}

    println(s"initial： $i, $j, $ind1, $ind2")

    if (! ind1 || ! ind2){
      ind1 = shorterArr( math.max(0, i - 1) ) <= longerArr(j)
      ind2 = longerArr( math.max(0, j - 1) ) <= shorterArr(i)
    }

    while (!ind1 || !ind2) {

      if (!ind1 && i > iMin) {
        iMax = i - 1
        println(s"Changed iMax: $iMax")
      }

      if (!ind2 && i < iMax) {
        iMin = i + 1
        println(s"Changed iMin: $iMin")
      }

      i = (iMin + iMax)/2

      j = (minLen + maxLen + 1)/2 - i

      if (i <= 0 && j >= maxLen -1) {i = 0; j = maxLen - 1; ind1 = true ; ind2 = true}
      else if (i >= minLen -1 && j <= 0) {i = minLen - 1; j = 0; ind1 = true ; ind2 = true}

      println(s"Current Value： $i, $j")

      if (! ind1 || ! ind2){
        ind1 = shorterArr( math.max(0, i - 1) ) <= longerArr(j)
        ind2 = longerArr( math.max(0, j - 1) ) <= shorterArr(i)
      }

    }

    if (shorterArr.isEmpty && !longerArr.isEmpty) longerArr(j).toDouble
    else if (longerArr.isEmpty && !shorterArr.isEmpty) shorterArr(i).toDouble
    else if (longerArr.isEmpty && shorterArr.isEmpty) 0.0
    else (shorterArr(i).toDouble + longerArr(j).toDouble)/2

  }

  // -------------------------- *** Problem: Array Partition I *** ----------------------

  def arrayPairSum(nums: Array[Int]): Int = {
    val sortArr = nums.sorted
    var ind = 0
    var ans = 0
    val maxLen = sortArr.length

    while (ind < maxLen){
      if ((ind + 1)%2 != 0){
        ans += sortArr(ind)
      }
      ind += 1
    }

    ans
  }

  // -------------------------- *** Problem: Sort Array by Parity *** ----------------------
  def sortArrayByParity(A: Array[Int]): Array[Int] = {
    var oddArr = Array[Int]()
    var evenArr = Array[Int]()
    var ind = 0
    val maxLen = A.length

    while (ind < maxLen){
      if (A(ind) % 2 != 0){oddArr = oddArr :+ A(ind)}
      else {evenArr = evenArr :+ A(ind)}
      ind  += 1
    }

    evenArr.filter(_ != null) ++ oddArr.filter(_ != null)
  }

  // -------------------------- *** Problem: Haming Distance *** ----------------------
  def hammingDistance(x: Int, y: Int): Int = {
    val bX = toBinary(x)
    val bY = toBinary(y)

    val minArr = if (bX.length > bY.length) bY else bX
    val maxArr = if (bX.length > bY.length) bX else bY

    val minLen = Math.min(bX.length, bY.length)
    val maxLen = Math.max(bX.length, bY.length)

    var ind = 0
    var count = 0

    while (ind < minLen){
//      println(s"Curent Position: $ind \n    minArr Current Val: ${minArr(ind)}, maxArr Current Val: ${maxArr(ind)}")
      if (minArr(ind) != maxArr(ind)) {count += 1}
//      println(s"    Current Count: $count")
      ind += 1
    }

    while (ind < maxLen){
//      println(s"Curent Position: $ind \n    Current Count: $count")
      count += maxArr(ind)
      ind += 1
    }

    count
  }

  // -------------------------- *** Problem: Flip Image *** ----------------------
  def flipArray (A: Array[Int]) = { //expect 0, 1 only

    val maxLen = A.length
    var ind = maxLen - 1
    var arr = Array[Int]()

    while (ind >= 0) {
      arr = arr :+ A(ind)
      ind -= 1
    }

    arr
  }

  def revertArray (A: Array[Int]) = {
    val maxLen = A.length
    var ind = 0
    var res = Array[Int]()
    while (ind < maxLen){
      res = res :+ math.abs(1 - A(ind))
      ind += 1
    }
    res
  }

  def flipAndInvertArray (A: Array[Int]) = {
    var ind = 0
    var backInd = A.length - 1
    val res = new Array[Int] (A.length)

    while (ind <= backInd){
      res(ind) = math.abs(A(backInd) - 1)
      res(backInd) = math.abs(A(ind) - 1)
      ind += 1
      backInd -= 1
    }

    res
  }

  def flipAndInvertImage (As: Array[Array[Int]]) = {
    As.map(arr => flipAndInvertArray(arr))
  }

  // -------------------------- *** Problem: InterSect Arrays *** ----------------------

  def intersection (a: Array[Int], b: Array[Int]) = {
//    a.toSet.intersect(b.toSet).toArray
    var mapped = Map[Int,Int]()
    val minArr = if (a.length > b.length) b else a
    val maxArr = if (a.length > b.length) a else b

    val minLen = minArr.length
    val maxLen = maxArr.length

    var ind = 0
    while (ind < minLen){
      mapped += (minArr(ind) -> 0)
      ind += 1
    }

    ind = 0
    while (ind < maxLen){
      val check = try {mapped(maxArr(ind))} catch {case e: Exception => -1}
      mapped = if (check == -1) mapped else mapped + (maxArr(ind) -> 2)
      ind += 1
    }

    mapped.filter(t => t._2 == 2).map(t => t._1).toArray

  }

  def intersectionByBit (nums1: Array[Int], nums2: Array[Int]): Array[Int] = {

    if (nums1 == null || nums2 == null) null
    else {
      val b1 = scala.collection.mutable.BitSet.empty
      val b2 = scala.collection.mutable.BitSet.empty
      for (i <- nums1) b1(i) = true
      for (i <- nums2) b2(i) = true
      (b1 & b2).toArray
    }
  }

  // -------------------------- *** Problem: Longest Continuous Increasing Subsequence *** ----------------------
  def findLongestClimbingSeq(nums: Array[Int]): Int = {

    var ind = 0
    val maxLen = nums.length
    var globalMax = math.min(maxLen, 1)

    while (ind + 1 < maxLen){
      var tf = true
      var count = 1
      while (ind + 1 < maxLen && tf) {
        tf = nums(ind + 1) > nums(ind)
        if (tf == true) count += 1
        ind += 1
      }
      globalMax = math.max(globalMax, count)
    }

    globalMax
  }

  def removeElement(nums: Array[Int], value: Int) = {
    var res = Array[Int]()
    var ind = 0
    val maxLen = nums.length

    while(ind < maxLen){
      if (nums(ind) != value ) res = res :+ nums(ind)
      ind += 1
    }

    res.length
  }

  // -------------------------- *** Problem: Longest Continuous Increasing Subsequence *** ----------------------

}
