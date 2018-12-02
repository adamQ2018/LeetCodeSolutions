import ArrayAlgorithms._
import LinkedListAlgorithms._
import StringAlgorithms._
import DataStructures._
import DataStructures.Node._
import scala.math._

object MathAlgorithm {

  // -------------------------- *** Problem: Haming Distance *** ----------------------

  def toBinary(x: Int)= {
    var ind = 0
    while (Math.pow(2,ind) < x){
      ind += 1
    }
    val arr = Array.fill(ind + 1)(0)

    var reminder = x.toDouble
    while (ind >= 0 && reminder != 0){
      val thisSubstract = reminder - Math.pow(2, ind)
      if (thisSubstract >= 0){
        reminder = thisSubstract
        arr(ind) = 1
      }
      ind -= 1
    }
    arr
  }

  def binaryToInt (binaryArr: Array[Int]): Int = {
    var ind = 0
    var res = 0
    while (ind < binaryArr.length - 1){
      res += (if (binaryArr(ind) == 1) pow(2, ind).toInt else 0)
      ind += 1
    }
    res
  }

  def countDiffBits(x: Int, y: Int) = {
    var count = 0
    var n = x^y
    while (n != 0){
      n = n & (n -1)
      count += 1
    }
    count
  }

  // -------------------------- *** Problem: Add Binary *** ----------------------
  def addBinaryWrapped(a: String, b: String): String = {
    binaryArrayToString(toBinary(binaryStrToNumber(a) + binaryStrToNumber(b)) )
  }

  def addBinary(a: String, b: String): String = {

    val longer = if (a.length > b.length) a else b
    val shorter = if (a.length > b.length) b else a

    var i = shorter.length - 1
    var j = longer.length - 1
    var res = ""
    var carry = 0

    while(i >= 0){
      if (longer(j) + shorter(i) + carry - '0' - '0' >= 2){
        res = longer(j) + shorter(i) + carry - '0' - '0' - 2 + res
        carry = 1
      }
      else{
        res = longer(j) + shorter(i) + carry - '0' - '0' + res
        carry = 0
      }
      i -= 1
      j -= 1
    }

    while (j >= 0){
      if ((longer(j) - '0' + carry) == 2){
        res = longer(j) + carry - '0' - 2 + res
        carry = 1
      }
      else{
        res = longer(j) - '0' + carry + res
        carry = 0
      }
      j -= 1
    }
    if (carry == 1) carry.toString + res else res
  }

  // -------------------------- *** Problem: Self Dividing Numbers *** ----------------------
  def isSelfDivide (x: Int): Boolean = {

    var track = x
    var ind = true

    while (track != 0 && ind){
      val lastDigit = track % 10
      ind = if (lastDigit != 0) x % lastDigit == 0  else false
//      println(s"Tracked Value: $track, Last Digit: $lastDigit, Current Result: $ind")
      track /= 10
    }

    ind
  }

  def findSelfDvdNums (low: Int, high: Int) = {

    var iter = low
    var arr = Array[Int]()

    while (iter <= high) {
      if (isSelfDivide(iter)) arr = arr :+ iter
      iter += 1
    }

    arr
  }

  // -------------------------- *** Problem: Mountain Single Peak *** ----------------------

  def findArrPeak (mountain: Array[Int]): Int = {

    val maxLen= mountain.length
    var backInd = maxLen - 1
    var ind = 0

    var tfIndBack = true
    var tfIndForward = true

    while (tfIndBack == true && tfIndForward == true && ind <= backInd){
      tfIndBack = mountain(backInd) < mountain(backInd - 1)
      tfIndForward = mountain(ind) < mountain(ind + 1)
      ind += 1
      backInd -= 1
    }

    if (!tfIndBack) backInd + 1 else if (!tfIndForward) ind - 1 else 0
  }

  // -------------------------- *** Problem: Count Primes *** ----------------------

  def countPrimes(n: Int): Int = {
    var num = 2
    val notPrime = new Array[Boolean](n)
    var count = 0

    while ( num < n){
      if ( notPrime(num) == false ) count += 1

      var base = 2
      while (base <= num && base*num < n){
        notPrime(base*num) = true
        base += 1
      }

      num += 1
    }

    count
  }

  // -------------------------- *** Problem: Happy Number - Number Digit Square Sum = 1 *** ----------------------
  def digitSqrSum (arr: Array[Char]): Int = {//Assume this is a number char only

    var ind = 0
    val maxLen = arr.length
    var res = 0

    while (ind < maxLen){
      val num = arr(ind).toString.toInt
      res += num*num
      ind += 1
    }
    res
  }

  def isHappyNumber(x: Int) = {

    var record = Array[Int]()
    var repeat = false
    var n = x

    while ((n != 1) && (repeat == false) ){
//      println(s"Current input value: $n")
      val numsArr = n.toString.toCharArray
//      println(s"Current Nums Array: ")
//      numsArr.foreach(println)
      repeat = record.contains(n)
      if (repeat != true) { record = record :+ n }
      n = digitSqrSum(numsArr)
//      println(s"Current Record Length: ${record.size}")
//      println(s"Current output value: $n")

    }

    n == 1
  }
  // -------------------------- *** Pascal's Triangle *** ----------------------
  def generatePascalTriangle (numRows: Int): List[List[Int]] = {
    if (numRows == 0){List()}
    else if ( numRows == 1 ) { List(List(1)) }
    else if ( numRows == 2 ){  List(List(1), List (1, 1)) }
    else {
      var res = List(List(1), List (1, 1))
      var ind = 2
      while (ind < numRows){
        val last = res.last
        var drvd = List[Int](1)
        var subInd = 1
        while (subInd < last.length){
          drvd = drvd :+ (last(subInd - 1) + last(subInd))
          subInd += 1
        }
        drvd = drvd :+ 1
        res = res :+ drvd
        ind += 1
      }
      res
    }
  }

  def printPascalTriangle (pascal: List[List[Int]]) = {
    val nrow = pascal.length
    var ind = 0
    while (ind < nrow){
      val skip = " " * (nrow - ind - 1)
      println(s"${skip + pascal(ind).mkString(" ") + skip}")
      ind += 1
    }
  }

  def multiple(n: Int): Int = {
    var res = 1
    var thisNum = 1
    while (thisNum <= n){
      res *= thisNum
      thisNum += 1
    }
    res
  }

  def multiple(start: Int, n: Int): Int = {
    var res = 1
    var thisNum = start
    while (thisNum <= n){
      res *= thisNum
      thisNum += 1
    }
    res
  }

  def binomNumber (ttl: Int, choose: Int): Int = {
    multiple(ttl - choose + 1, ttl)/multiple(choose)
  }

  def pascalByBinom (nrow: Int) ={

    var thisRow = 0
    var res = List[List[Int]]()

    while (thisRow < nrow){
      var ind = 0
      var thisList = List[Int]()
      while (ind <= thisRow){
//        println(s"Row $thisRow, Position: $ind, value: ${binomNumber(thisRow, ind)}")
        thisList = thisList :+ binomNumber(thisRow, ind)
        ind += 1
      }
      res = res :+ thisList
      thisRow += 1
    }

    if (nrow != 0) res else List()
  }

  // -------------------------- *** Climb Stairs *** ----------------------

  def fibonacciNumber(max: Int): Int ={
    if (max == 0) { 1 }
    else if (max == 1) { 1 }
    else if  (max == 2) { 2 }
    else {
      var ind = 2
      var first = 1
      var second = 2
      while (ind < max){
        second = first + second
        first = second - first
        ind += 1
      }
      second
    }
  }

  def fibonacciNumberByFormula (max: Int): Int = {
    if (max == 0) {1}
    else if (max == 1) {1}
    else if  (max == 2) { 2 }
    else {
     (   1/math.sqrt(5) *
       (
         math.pow(  ( math.sqrt(5) + 1 )/2 , max + 1) - math.pow( ( 1 - math.sqrt(5) )/2 , max + 1 )
         )
       ).toInt
    }
  }

  // -------------------------- *** Is Perfect Square ? *** ----------------------
  def isPerfectSq (n: Int) = {//Newton's Method
    if (n == 1) { true }
    else if (n < 1) { false }
    else {
      var x: Long =  n / 2

      while (x * x > n){
        x = (x + n / x)/2

      }
//      println(x)
      x * x == n
    }
  }

  // -------------------------- *** Binary Search *** ----------------------
  def binarySearch (nums: Array[Int], target: Int): Int = {
    var high = nums.length - 1
    var low = 0
    var ind = false
    var mid = 0

    while (low <= high && ind == false){
      mid = (low + high)/2
      if (nums(mid) < target){
        low = mid + 1
      }
      else if (nums(mid) > target){
        high = mid - 1
      }
      else (ind = true)
    }

    if (ind == true) mid else -1
  }

  // -------------------------- *** isPowerOfTwo *** ----------------------
  def isPowerOfTwo(n: Int): Boolean = {
    var i = 0
    while (pow(2,i) < n){
      i += 1
    }
    pow(2,i) == n
  }

  def isPowerOfTwoBinary(n: Int) = {
    n > 0 && (n & (n - 1)) == 0
  }

  // -------------------------- *** Number Complement in Binary *** ----------------------
  def findComplement(num: Int): Int = {
    val binaryArr = toBinary(num)
    val complimentArr = revertArray(binaryArr)
    binaryToInt(complimentArr)
  }

  // -------------------------- *** Problem: Add Digits *** ---------------------
  def addDigits(num: Int): Int = {

    var sum = num
    var trace = sum

    while(sum >= 10){
      trace = sum
      sum = 0
      while (trace != 0){
        sum += trace%10
        trace = trace/10
      }
      println(s"Current Digit Sum: $sum")
    }

    sum
  }

  // -------------------------- *** Problem: Single Number *** ---------------------
  def onlySingleNumber(nums: Array[Int]): Int = {

    if (nums.isEmpty){
      0
    }
    else{

      var ind = 1
      var tracker = nums(0)

      while (ind < nums.length){
        tracker = nums(ind) ^ tracker
        ind += 1
      }
      tracker
    }
  }

  // -------------------------- *** Problem: Good Number *** ---------------------
  def rotatedDigits(N: Int): Int = {
    val must = Array( '2', '5', '6', '9' )
    val avoid = Array( '3', '4', '7' )

    var num = 1
    var count = 0

    while (num <= N){
      val thisNumber = num.toString.toCharArray
      val cantHaveCheck = thisNumber.foldLeft(false)( (tf, x) => tf || avoid.contains(x))
      val mustHaveCheck = thisNumber.foldLeft(false)( (tf, x) => tf || must.contains(x))
      if (!cantHaveCheck && mustHaveCheck){
        count += 1
      }
      num += 1
    }

    count
  }

  // -------------------------- *** Problem: Count Binary Substrings *** ---------------------
  def countBinarySubstrings(s: String): Int = {

    var ind = 1
    var currentLen = 1
    var prevLen = 0
    var count = 0

    while (ind < s.length){
      if ( s(ind) == s(ind -1) ){
        currentLen += 1 // this means current 1s or 0s does not end
      }
      else { // this means current 0s or 1s ends
        prevLen = currentLen
        currentLen = 1
      }
      if (currentLen <= prevLen){ // if this holds, there is always a match in the previous companion number seq
        count += 1
      }
      ind += 1
    }
    count
  }


  // -------------------------- *** Problem: Valid Parentheses *** ---------------------
  def symbolEncoder (sym: Char) = {

    sym match {
      case '{' => 1
      case '}' => -1
      case '[' => 2
      case ']' => -2
      case '(' => 3
      case ')' => -3
      case _ => throw new IllegalArgumentException("Not Valid Symbol")
    }
  }

  def isParenthesesValid(s: String): Boolean = { //last in first out

    var tf = true
    var ind = 0
    var arr = s.toCharArray
    var stack = new Stack[Int]()

    while (ind < arr.length && tf){
      val thisChar = symbolEncoder(arr(ind))
      if (thisChar > 0){
        stack.push(thisChar)
      }
      else{
        try {val last = stack.pop(); tf = thisChar == -1 * last}
        catch {case e: Exception => tf = false}
      }
      ind += 1
    }

    tf && stack.isEmpty
  }

  // -------------------------- *** Problem: Container With Most Water *** ---------------------
  def maxArea(height: Array[Int]): Int = {

    var left = 0
    var right = height.length - 1
    var max = 0
    var i = height.length - 1

    while (i >= 0){
      if (height(left) < height(right)){
        max = math.max(max, height(left) * i)
        left += 1
      }
      else{
        max = math.max(max, height(right) * i)
        right -= 1
      }
      i -= 1
    }
    max
  }

  // -------------------------- *** Problem: Friend Circle *** ---------------------
  def findAndRemove(M: Array[Array[Int]], i: Int): Unit ={
    var j = 0
    while (j < M.length){
      if (M(i)(j) == 1){
        M(i)(j) = 0; M(j)(i) = 0
        findAndRemove(M, j)
      }
      j += 1
    }
  }

  def findCircleNum(M: Array[Array[Int]]): Int = {

    var i = 0
    var numCircle = 0

    while (i < M.length){
      if (M(i)(i) == 1){
        findAndRemove(M, i)
        numCircle += 1
      }
      i += 1
    }
    numCircle
  }

  // -------------------------- *** Problem: Number Islands *** ---------------------
  def findAndRemoveDirectEdge(M: Array[Array[Char]], i: Int, j: Int): Unit ={
    println(s"entered with: $i, $j")
    M(i)(j) = '0'
    if (i < M.length && j < M.last.length - 1 && M(i)(j + 1) == '1'){
      M(i)(j + 1) = '0'
      findAndRemoveDirectEdge(M, i, j+1)
    }
    if (i < M.length - 1 && j < M.last.length && M(i+1)(j) == '1'){
      M(i+1)(j) = '0'
      findAndRemoveDirectEdge(M, i+1, j)
    }
    if (i >= 1 && j < M.last.length && M(i - 1)(j) == '1'){
      M(i - 1)(j) = '0'
      findAndRemoveDirectEdge(M, i - 1, j)
    }
    if (i < M.length && j >= 1 && M(i)(j - 1) == '1'){
      M(i)(j - 1) = '0'
      findAndRemoveDirectEdge(M, i, j - 1)
    }
  }

  def numIslands(M: Array[Array[Char]]): Int = {

    var i = 0
    var nIsland = 0

    while (i < M.length){
      var j = 0
      while(j < M.last.length){
        if(M(i)(j) == '1') {findAndRemoveDirectEdge(M, i, j); nIsland += 1}
        j += 1
      }
      i += 1
    }
    nIsland
  }

  // -------------------------- *** Problem: Poor Pigs *** ---------------------
  def poorPigs(buckets: Int, minutesToDie: Int, minutesToTest: Int): Int = {
    ( math.log(buckets)/( math.log(minutesToTest/minutesToDie + 1) ) ).toInt
  }

  // -------------------------- *** Problem: Power of 3 *** ---------------------
  def isPowerOfThree(n: Int): Boolean = { // this only works for integers
    n > 0 && math.pow(3, 33)%n == 0
  }

  // -------------------------- *** Problem: Nth Digit *** ---------------------
  def findNthDigit(n: Int): Int = {

    var digits: Long = 1
    var base: Long = 9
    var input: Long = n
    var levelBound: Long = 0
    // find digit
    while(input > base*digits){
      input -= base * digits
      base = base * 10
      levelBound = levelBound * 10 + 9
      digits += 1
    }//O(1)

    println(s"Deduced digit level $digits, last boundary number: $levelBound")
    var nthDigit = input % digits

    val number: Long = if (nthDigit == 0){
      nthDigit = digits
      input/digits + levelBound //this will give you the number at this base level e.g. 11-9 =2 /2 = 1 => this is the first number in 2-digit numbers, which is 10
    }
    else{
      input/digits + 1 + levelBound
    }

    println(s"Deduced number: $number, need $nthDigit th digit")

    ((number/math.pow(10, (digits - nthDigit)).toLong) % 10).toInt
  }


}
