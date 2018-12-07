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

  // -------------------------- *** Problem: Move Zeros *** ----------------------
  def moveZeroes(nums: Array[Int]): Array[Int] = {

    var ind = 0
    var zeroLoc = 0
    val res = nums

    while (ind < res.length){
      if (res(ind) != 0){
        val temp = res(ind) + res(zeroLoc)
        res(ind) = res(zeroLoc)
        res(zeroLoc) = temp - res(ind)
        zeroLoc += 1
      }
      ind += 1
    }
    res
  }

  // -------------------------- *** Problem: Find All Numbers Disappeared in an Array *** ----------------------
  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
    var res = List[Int]()
    var ind = 0

    while (ind < nums.length){
      if ( nums(ind) ==  -1 || nums(ind) == (ind + 1) ){
        ind += 1
      }
      else {
        if (nums(nums(ind) - 1) == nums(ind)){
          nums(ind) = -1
          ind += 1
        }
        else{
          val temp = nums(nums(ind) - 1)
          nums(nums(ind) - 1) = nums(ind)
          nums(ind) = temp
        }
      }
    }

    ind = 0
    while (ind < nums.length){
      if(nums(ind) == -1){ res = res :+ (ind + 1)}
      ind += 1
    }
    res
  }

  // -------------------------- *** Problem: Fair Candy Swap *** ---------------------
  def fairCandySwap(A: Array[Int], B: Array[Int]): Array[Int] = {
    val diff = (A.sum - B.sum)/2
    var mapped = scala.collection.mutable.Map[Int, Int]()

    var ind = 0
    while (ind < A.length){
      mapped += ( (A(ind) - diff) -> A(ind) )
      ind += 1
    }

    ind = 0
    while (ind < B.length
      && ( try{mapped(B(ind)); false} catch{case e: Exception => true})
    ){
      ind += 1
    }

    Array( mapped(B(ind)), B(ind))
  }

  // -------------------------- *** Problem: Maximum Subarray *** ---------------------
  def maxSubArray(nums: Array[Int]): Int = {
    var globalMax = Int.MinValue
    var ind = 0
    var maxSoFar = 0

    while (ind < nums.length){
      // println(s"current number: ${nums(ind)}, max so far: $globalMax")
      maxSoFar += nums(ind)
      // println(s"Current Step resulted max: $globalMax")
      globalMax = math.max(globalMax, maxSoFar)
      if (maxSoFar < 0) maxSoFar = 0

      ind += 1
    }
    globalMax
  }

  // -------------------------- *** Problem: Fruit Into Baskets *** ---------------------
  def totalFruit(tree: Array[Int]): Int = {
    var iter = 1
    var maxLen = 1
    var recorded = scala.collection.mutable.ArrayBuffer[Int](2)
    recorded = recorded :+ tree(0)
    var instantMax = 1
    var newStart = 0


    while (iter < tree.length){
      // println(s"Current value is ${tree(iter)}")
      if ( recorded.contains( tree(iter) ) || recorded.size < 2){ // if the value is not a new distinct or if the count of distinct is smaller than 2
        if (tree(iter) != tree(iter - 1)){ newStart = iter }
        if (recorded.size == 1) recorded = recorded :+ tree(iter) // update the last encountered value position
        // println(s"without refreshing the map,\n    current map size ${mapped.size}")
        instantMax += 1 //no matter how, once the parent "if" satisfied, increase maxLen by 1
        iter += 1 //increase the iteration by 1
      }
      else {
        // println(s"Now need to refresh the map, current value is ${tree(iter)}")
        instantMax = iter - newStart + 1//now maxLen is starting from the last to the current
        recorded = scala.collection.mutable.ArrayBuffer(tree(iter - 1), tree(iter)) // reassign the map
        newStart = iter
        // println(s"After refreshing, current instantMax is $instantMax, achieved from recorded last distinct: ${newStart}, current Map Size is ${mapped.size}")
        iter += 1
      }
      maxLen = math.max(maxLen, instantMax)
    }
    maxLen
  }

  // -------------------------- *** Problem: Three Sum *** ---------------------
  def twoSumWithRestriction(arr: Array[Int], target: Int, banned: Int): Array[Array[Int]] = {
    var mapped = scala.collection.mutable.Map[Int, Int]()
    var ind = 0
    var companion = 0
    var res =  Array[Array[Int]]()

    while (ind < arr.length ){
      if (ind != banned){
        companion = target - arr(ind)
        if ( mapped.contains(companion) ){ res = res :+ Array(arr(ind), companion) }
        else {  mapped += (arr(ind) -> ind) }
      }
      ind += 1
    }
    res
  }


  def threeSum(arr: Array[Int], target: Int): Array[Array[Int]] = {

    var i = 0
    var recorded = scala.collection.mutable.Map[String, Boolean]()
    var res = Array[Array[Int]]()

    while (i < arr.length){
      val tempRes = twoSumWithRestriction(arr, target - arr(i), i)
      if (!tempRes.isEmpty){
        var j = 0
        while (j < tempRes.length){
          val thisRes = (tempRes(j) :+ arr(i)).sorted
          if ( !recorded.contains(thisRes.mkString(",")) ){
            recorded += (thisRes.mkString(",") -> true)
            res = res :+ thisRes
          }
          j += 1
        }
      }
//      println(s"current solution to input ${arr(i)} \n    ${tempRes.map(x => x.mkString(",")).mkString("|")}")
      i += 1
    }
    res
  }

  // -------------------------- *** Problem: Merge Sorted Array *** ---------------------
  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {

      var reverseInd = m + n - 1
      var i = m - 1
      var j = n - 1

      while(reverseInd >= 0){

        while(  i >= 0 && ( j == -1 || nums1(i) > nums2(j) )  ){
          nums1(reverseInd) = nums1(i)
          reverseInd -= 1
          i -= 1
        }

        while(  j >= 0 && ( i == -1 || nums1(i) <= nums2(j) )  ){
          nums1(reverseInd) = nums2(j)
          reverseInd -= 1
          j -= 1
        }
      }
  }

  // -------------------------- *** Problem: Best Time to Buy and Sell Stock II *** ---------------------
  def maxProfit(prices: Array[Int]): Int = {

    var profit = 0
    var i = 1

    while(i < prices.length){
      profit += math.max(0, prices(i) - prices(i - 1))
      i += 1
    }

    profit
  }

  // -------------------------- *** Problem: Majority Element *** ---------------------
  def partition (A: Array[Int], start: Int, end: Int) = {
    val base = A(end)
    var i = start - 1
    var j = start

    while (j < end) {
      if(A(j) < base) {
        i += 1
        val temp = A(j)
        A(j) = A(i)
        A(i) = temp
      }
      j += 1
    }
    val temp = A(end)
    A(end) = A(i + 1)
    A(i + 1) = temp
    i + 1
  }

  def quickSort(A: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val pivot = partition(A, start, end)
      quickSort(A, start, pivot - 1)
      quickSort(A, pivot + 1, end)
    }
  }

  def majorityElementBySort(nums: Array[Int]): Int = {
    quickSort(nums, 0, nums.length - 1)
    nums(nums.length/2)
  }

  def majorityElementByHashMap(nums: Array[Int]): Int = {

    val record = scala.collection.mutable.Map[Int, Int]()
    var i = 0
    var max = 0
    var maxNum = 0

    while (i < nums.length){
      if( record.contains(nums(i)) ){
        println(s"Record Contains this Number: ${nums(i)}, the count is ${record(nums(i))}")
        record +=  (nums(i) -> (record(nums(i)) + 1) )
        println(s"Updated Count for this Number: ${nums(i)}, the count becomes ${record(nums(i))}")
      }
      else {
        record += (nums(i) -> 1)
        println(s"Record Does Not Contain this Number: ${nums(i)}, added count to be ${record(nums(i))}")
      }

      if (record(nums(i)) > max){
        println(s"The current count is ${record(nums(i))}, larger than the current max: $max")
        max = record(nums(i))
        maxNum = nums(i)
        println(s"Max has been updated to be $max, max counted number becomes $maxNum")
      }

      i += 1
    }
    maxNum
  }

  def majorityElement(nums: Array[Int]): Int = {

    var count = 1
    var res = nums(0)
    var i = 1

    while(i < nums.length){
      if(count == 0){
        res = nums(i)
        count = 1
      }
      else if (res == nums(i)){
        count += 1
      }
      else{
        count -= 1
      }
      i += 1
    }
    res
  }

  // -------------------------- *** Problem: Find All Duplicates in an Array *** ---------------------
  def findDuplicates(nums: Array[Int]): List[Int] = {

    var i = 0
    var loc = 0
    var res = List[Int]()

    while(i < nums.length){
      loc = math.abs(nums(i)) - 1
      if (nums(loc) < 0){
        res = res :+ math.abs(nums(i))
        nums(loc) = - nums(loc)
      }
      else{
        nums(loc) = - nums(loc)
      }
      i += 1
    }
    res
  }

  // -------------------------- *** Problem: Sort Array by Parity *** ---------------------
  def sortArrayByParityII(A: Array[Int]): Array[Int] = {

    val input = A
    var oddLoc = 1
    var evenLoc = 0

    while (oddLoc < A.length && evenLoc < A.length - 1){
      while (oddLoc < A.length && input(oddLoc)%2 !=0){
        oddLoc += 2
      }
      while (evenLoc < A.length - 1 && input(evenLoc)%2 ==0){
        evenLoc += 2
      }
      if (oddLoc < A.length && evenLoc < A.length - 1){
        val temp = input(oddLoc)
        input(oddLoc) = input(evenLoc)
        input(evenLoc) = temp
        oddLoc += 2
        evenLoc += 2
      }
    }
    input
  }

  // -------------------------- *** Problem: Find Anagram Mappings *** ---------------------
  def anagramMappings(A: Array[Int], B: Array[Int]): Array[Int] = {

    var hashed = scala.collection.mutable.HashMap[Int, Array[Int]]()
    var i = 0

    while (i < B.length){
      if (hashed.contains(B(i))){
        hashed += ( B(i) -> (hashed(B(i)) :+ i) )
      }
      else{
        hashed += ( B(i) -> Array(i) )
      }
      i += 1
    }

    i = 0
    var res = Array[Int]()

    while (i < A.length){
      res = res :+ hashed(A(i)).last
      hashed += ( A(i) -> hashed(A(i)).dropRight(1) )
      i += 1
    }
    res
  }

  // -------------------------- *** Problem: Next Greater Element I *** ---------------------
  def nextGreaterElement(toFind: Array[Int], base: Array[Int]): Array[Int] = {

    val mapped = scala.collection.mutable.HashMap[Int, Int]()
    val stacked = new Stack[Int]()
    var i = 0
    val res = Array.fill(toFind.length)(-1)

    while (i < base.length){
      while(!stacked.isEmpty && stacked.peek < base(i)){
        mapped += (stacked.pop -> base(i))
      }
      stacked.push(base(i))
      i += 1
    }

    i = 0

    while(i < toFind.length){
      if (mapped.contains(toFind(i))) res(i) = mapped(toFind(i))
      i += 1
    }

    res
  }

  // -------------------------- *** Problem: Degree of an Array *** ---------------------
  def findShortestSubArray(nums: Array[Int]): Int = {

    val countMap = scala.collection.mutable.HashMap[Int, Int]()
    val startMap = scala.collection.mutable.HashMap[Int, Int]()
    val endMap = scala.collection.mutable.HashMap[Int, Int]()
    var i = 0
    var maxCnt = 0
    var shtst = 0

    while(i < nums.length){
      if ( ! countMap.contains(nums(i)) ){
        startMap += (nums(i) -> i)
        countMap += (nums(i) -> 1)
        endMap += (nums(i) -> i)
        if (maxCnt < 1){
          maxCnt = 1
          shtst = 1
        }
      }
      else{
        val thisCnt = countMap(nums(i)) + 1
        countMap += ( nums(i) -> thisCnt)
        endMap += (nums(i) -> i)
        val thisLen = endMap(nums(i)) - startMap(nums(i)) + 1
        if (maxCnt < thisCnt){
          maxCnt = thisCnt
          shtst = thisLen
        }
        else {
          if (maxCnt == thisCnt && shtst > thisLen){
            shtst = thisLen
          }
        }
      }
      i += 1
    }
    shtst
  }

  // -------------------------- *** Problem: Top K Frequent Elements *** ---------------------
  def topKFrequent(nums: Array[Int], k: Int): List[Int] = {

    var mapped = scala.collection.mutable.HashMap[Int, Int]()
    var i = 0

    while (i < nums.length){
      if (mapped.contains(nums(i))){
        mapped += ( nums(i) -> (mapped(nums(i)) + 1) )
      }
      else{
        mapped += ( nums(i) -> 1 )
      }
      i += 1
    }

    mapped.toList.sortBy(_._2).map(t => t._1).takeRight(k)
  }

  // -------------------------- *** Problem: Can Place Flowers *** ---------------------
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {

    var i = 0
    var free = 0

    while (i < flowerbed.length){

      if (flowerbed(i) == 1){
        i += 2
      }
      else{
        if (i - 1 >= 0 && i + 1 < flowerbed.length){
          if(flowerbed( i + 1 ) == 0 && flowerbed( i - 1 ) == 0 ) {free += 1;  i += 2}
          else {i += 1}
        }
        else if (i + 1 == flowerbed.length && i - 1 >= 0 && flowerbed( i - 1 ) == 0 ) {free += 1;  i += 2}
        else if (i - 1 < 0 && i + 1 < flowerbed.length && flowerbed( i + 1 ) == 0 ) {free += 1;  i += 2}
        else if (i - 1 < 0 && i + 1 == flowerbed.length) {free += 1;  i += 2}
        else {i += 1}
      }
    }
    free >= n
  }

  // -------------------------- *** Problem: Array Permutation *** ---------------------
  def permute(nums: Array[Int]): List[List[Int]] = {
    var k = nums.length
    var res = List[List[Int]]()
    if (k > 2){
      var i = 0
      while(i < nums.length){
        val tempRes = permute(nums.drop(i + 1) ++ nums.dropRight(nums.length - i))
        var j = 0
        while (j < tempRes.length){
          res = res :+ ( nums(i) +: tempRes(j) )
          j += 1
        }
        i += 1
      }
    }
    else{
      if (k == 2) res = List(List(nums(0), nums(1)), List(nums(1), nums(0)))
      else if (k == 1) res = List(List(nums(0)))
      else res = List(List())
    }
    res
  }

  // -------------------------- *** Problem: Array Permutation *** ---------------------

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {

    var area = 0
    val maxI = grid.length - 1
    val maxJ = grid.last.length - 1
    var maxArea = 0

    def addAreaAndRemove(grid: Array[Array[Int]], i: Int, j: Int): Unit = {
      if (i + 1 <= maxI && j <= maxJ && grid(i + 1)(j) == 1){
        area += 1
        grid(i + 1)(j) = 0
        addAreaAndRemove(grid, i + 1, j)
      }
      if (i - 1 >= 0 && j <= maxJ && grid(i - 1)(j) == 1){
        area += 1
        grid(i - 1)(j) = 0
        addAreaAndRemove(grid, i - 1, j)
      }
      if (j + 1 <= maxJ && i <= maxI && grid(i)(j + 1) == 1){
        area += 1
        grid(i)(j + 1) = 0
        addAreaAndRemove(grid, i, j + 1)
      }
      if (j - 1 >= 0 && i <= maxI && grid(i)(j - 1) == 1){
        area += 1
        grid(i)(j - 1) = 0
        addAreaAndRemove(grid, i, j - 1)
      }
    }

    var i = 0

    while (i <= maxI){
      var j = 0
      while (j <= maxJ){
        if (grid(i)(j) == 1){
          grid(i)(j) = 0
          area = 1
          addAreaAndRemove(grid, i, j)
        }
        maxArea = math.max(area, maxArea)
        j += 1
      }
      i += 1
    }
    maxArea
  }

  // -------------------------- *** Problem: Maximal Rectangle *** ---------------------
  def maximalRectangle(matrix: Array[Array[Int]]): Int = {
    //set up
    val m = matrix.length
    val n = matrix.last.length
    val dpLeft = Array.fill(n)(0)
    val dpRight = Array.fill(n)(n)
    val dpHeight = Array.fill(n)(0)
    var maxArea = 0
    var i = 0

    while (i < m){
      println(s"current row: $i")
      var j = 0
      var leftEnd = 0
      var rightEnd = n

      while (j < n){
        if (matrix(i)(j) == 1){
          println(s"detected entry 1 at $i, $j -> last height: ${dpHeight(j)}, last left end: ${dpLeft(j)}")
          dpLeft(j) = math.max(dpLeft(j), leftEnd)
          dpHeight(j) = dpHeight(j) + 1
          println(s"updated record at $i, $j -> last height: ${dpHeight(j)}, last left end: ${dpLeft(j)}")
        }
        else{
          dpLeft(j) = 0; leftEnd = j + 1
          dpHeight(j) = 0
        }
        j += 1
      }

      j -= 1

      while(j >= 0){
        if (matrix(i)(j) == 1){
          println(s"detected entry 1 at $i, $j -> last right: ${dpRight(j)}")
          dpRight(j) = math.min(dpRight(j), rightEnd)
          println(s"updated record at $i, $j -> last right end: ${dpRight(j)}")
        }
        else{
          dpRight(j) = n; rightEnd = j //no need to minus one because based on index, it's right - left + 1 (e.g. 0, 1, 2 has length 3)
        }
        j -= 1
      }

      j += 1

      while(j < n){
        println(s"Current largest area $maxArea, candidate area: ${(dpRight(j) - dpLeft(j))*dpHeight(j)}")
        maxArea = math.max(maxArea, (dpRight(j) - dpLeft(j))*dpHeight(j))
        j += 1
      }
      i += 1
    }
    maxArea
  }
}
