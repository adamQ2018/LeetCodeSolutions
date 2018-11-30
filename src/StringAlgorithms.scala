import ArrayAlgorithms._
import LinkedListAlgorithms._
import StringAlgorithms._
import DataStructures._
import DataStructures.Node._
import TreeAlgorithm._
import MathAlgorithm._
import scala.util.matching.Regex._

object StringAlgorithms {

  // -------------------------- *** Problem: Longest Non-repeat Substring *** ----------------------

  def findMaxDstnctSubStr (str: String): (String, Int) = {

    var map = Map[Char, Int]()

    val maxLen = str.length
    var ind = 0
    var currentMax = 0
    var currentBestStr = ""

    //    var cureentStart = 0

    var res = 0
    var resStr = ""

    var thisRepeatStart = 0

    while (ind < maxLen) {

      val thisInd = try {map(str(ind))} catch {case e: Exception => -1}

      //      println(s"current pos: $ind")

      if (thisInd == -1) {
        map = map ++ Map(str(ind) -> ind)
        currentMax += 1
        currentBestStr = currentBestStr + str(ind)
        //        println(s"current max: $currentMax")
        if (currentMax >= res) {
          res = currentMax
          resStr = currentBestStr
        }

      }
      else {

        thisRepeatStart = map( str(ind) )
        //        println(s"repeat pos: $thisRepeatStart")
        map = map.filter(mapping => mapping._2 > thisRepeatStart)
        map = map ++ Map(str(ind) -> ind)
        currentMax = ind - thisRepeatStart
        //        println(s"Recalculating current max: $currentMax")
        currentBestStr = map.map(t => "" + t._1).reduce(_ + _)
      }
      ind += 1
    }

    (resStr, res)
  }

  // -------------------------- *** Problem: Count and Say *** ----------------------

  val numToStringMap = Map(
    0 -> "zero",
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine"
  )

  val stringToNumMap = Map(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def readNumber(num: Long): String = {
    val strExpr = num.toString
    var ind = 0
    var count = 0
    var res = ""

    var lastNum = strExpr(0)
    var thisNum = strExpr(0)

    val maxLen = strExpr.length

    while (ind < maxLen){

      while (thisNum == lastNum && ind < maxLen){
        count += 1
        ind += 1
        if (ind < maxLen) thisNum = strExpr(ind)
      }
      res += numToStringMap(count) + " " + lastNum + " "
//      println(s"so far the iterator is: $ind")
//      println(s"So far the read is: $res")
      lastNum = thisNum
      count = 0

    }

    res
  }

  def readNumString (str: String): Long = {
    val arr = str.split(" ")
    arr
      .map( word => {
      try{stringToNumMap(word).toString} catch {case e: Exception => word}
    })
      .reduce(_ + _)
      .toLong
  }

  def countAndSayStraight(target: Int): String = {
    var res = "1"
    var ind = 1
    while (ind < target){
      val thisNum = readNumString(res)
      res = readNumString(readNumber(thisNum)).toString
      ind += 1
    }
    res
  }

  def countAndSay (target: Int): String = {
    var res = "1"
    var ind = 1

    while (ind < target){

      var countInd = 0
      var thisNum = res(0)
      var lastNum = res(0)
      var thisRes = ""

      while (countInd < res.length){

        var count = 0

        while(countInd < res.length && thisNum == lastNum){
          countInd += 1
          count += 1
          if(countInd < res.length) thisNum = res(countInd)
        }

        thisRes = thisRes + count.toString + lastNum
//        println(s"Current attemp $ind -> Res: $thisRes")
        lastNum = thisNum
      }

      ind += 1
      res = thisRes
//      println(s"Run Number: $ind -> $res")
    }
    res
  }

  // -------------------------- *** Problem: Longest Palindrome *** ----------------------

  def evenExpandAroundCenter(str: String, pos: Int): Int ={
    var count = 0
    var leftPos = pos - 1
    var rightPos = pos + 1

    while (leftPos >= 0 && rightPos < str.length && str(leftPos) == str(rightPos)){
//      println(s"current center: ${str(pos)}")
      count += 1
      leftPos -= 1
      rightPos += 1
//      println(count)
    }
    count * 2 + 1
  }

  def skewExpandAroundCenter(str: String, pos: Int): Int ={
    var count = 0
    var leftPos = pos - 1
    var rightPos = pos

    while (leftPos >= 0 && rightPos < str.length && str(leftPos) == str(rightPos)){
      //      println(s"current center: ${str(pos)}")
      count += 1
      leftPos -= 1
      rightPos += 1
      //      println(count)
    }
    count * 2
  }

  def longestPalindrome (str: String): String = {

    var ind = 0
    var maxLen = 0
    var bestPos = 0
    var res = ""

    while (ind < str.length){
      val evenMax = evenExpandAroundCenter(str, ind)
      val skewMax = skewExpandAroundCenter(str, ind)
      val thisMax = math.max(evenMax, skewMax)
//      println(s"this max: $thisMax")
      if (thisMax > maxLen) {
        maxLen = thisMax
        bestPos = ind
        res = if (evenMax >= skewMax) str.substring(bestPos - (maxLen - 1)/2, bestPos + (maxLen - 1)/2 +  1) else str.substring(bestPos - maxLen/2, bestPos + maxLen/2 )
      }
      ind += 1
//      println(s"current best: $bestPos")
//      println(s"current max Length: $maxLen")
    }

//    println(s"result max Length: $maxLen")
//    println(s"result bestPos: $bestPos")

    res

  }

  // -------------------------- *** Problem: Reverse Int *** ----------------------

  def revertInt (x: Int): Int = {
    val res =
    try {
      var initial = x
      while (initial/10 == initial.toDouble/10 && initial != 0){
        initial = initial/10
      }
      if (x < 0) -1 * math.abs(initial).toString.reverse.toInt
      else initial.toString.reverse.toInt
    }
    catch {case e: Exception => 0}

    res
  }

  // -------------------------- *** Problem: ZigZag Print *** ----------------------

  def zigzagTransform (str: String, nrow: Int): String = {

    val arr = Array.fill(nrow)("")
    var ind = 0
    var rowInd = 0
    val maxLen = str.length

    while (ind < maxLen){
      while (rowInd < nrow && ind < maxLen){
        arr(rowInd) = arr(rowInd) + str(ind)
//        println(s"Row $rowInd:${arr(rowInd)}")
        ind += 1
        rowInd += 1
      }
      rowInd = math.max(nrow - 2, 0)
      while (rowInd > 0 && ind < maxLen){
        arr(rowInd) = arr(rowInd) + str(ind)
//        println(s"Row $rowInd:${arr(rowInd)}")
        ind += 1
        rowInd -= 1
      }

    }

    arr.reduce(_ + _)
  }

  // -------------------------- *** Problem: atoi implementation *** ----------------------

  def myAtoi (str: String): Int = {

    var res = 0
    var numStr = ""
    var maxLen = str.length
    var sign = 1
    var start = 0

    val initial = if (maxLen == 0){" "} else {
      var ind = 0
      while (ind < maxLen && ("" + str(ind)) == " ") {
        ind += 1
      }
      str.substring(ind, maxLen)
    }

//    println(s"initial: $initial")

    maxLen = initial.length

    if (maxLen == 0){
      res = 0
    }
    else if (
      (try {("" + initial(0)).toInt} catch {case e: Exception => -10})
        == -10 && ("" + initial(0)) != "+" && ("" + initial(0)) != "-") {
      res = 0
    }
    else {
      if (("" + initial(0)) == "+"){sign = 1; start = 1}
      if (("" + initial(0)) == "-"){sign = -1; start = 1}
      var thisVal = 0
      while (thisVal != -10 && start < maxLen){
        thisVal = try {("" + initial(start)).toInt} catch {case e: Exception => -10}
        numStr = numStr + (if (thisVal != -10) thisVal.toString else "")
//        println(s"Current Number: $thisVal, current numStr: $numStr")
        start += 1
      }
//      println(sign)
      if (numStr.length == 0) {res = 0}
      else {res =
        try{
          if (sign < 0) -1 * numStr.toInt else numStr.toInt
        }
        catch {
          case e: Exception if sign < 0 => Int.MinValue
          case e: Exception if sign > 0 => Int.MaxValue
      }

      }
    }
    res
  }

  // -------------------------- *** Problem: Valid Number *** ----------------------

  def isNumber(s: String): Boolean = {

    val input = s.replace(" ", "")

    val validSeq = Seq(
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "e",
      "+",
      "-",
      "."
    )
    val check = input.foldLeft (true) ( (x, y) => x && validSeq.contains("" + y) || ("" + y) == " ")

    if (!check){
      false
    }
    else{
      try {input.toDouble; true} catch {case e: Exception => false}
    }
  }

  // -------------------------- *** Problem: Palindrome Number *** ----------------------
  def isPalindrome(x: Int): Boolean = {

//    x.toString.reverse == x.toString // Simple but slow
    var res = true
    if (x < 0){
      res = false
    }
    else
    {
      val thisVal = x.toString
      val len = thisVal.length
      var start = 0
      var end = len - 1
      if (len % 2 != 0){
        while (start != end && res == true){
          res = res && thisVal(start) == thisVal(end)
          start += 1
          end -= 1
        }
      }
      else{
        while (start != (end + 1) && res == true){
          res = res && thisVal(start) == thisVal(end)
          start += 1
          end -= 1
        }
      }

    }
    res
  }

  // -------------------------- *** Problem: Int To Roman *** ----------------------

  def toRome (x: Int): String = {
    val romeMap = Map(
      0 -> "",
      1 -> "I",
      5 -> "V",
      10 -> "X",
      50 -> "L",
      100 -> "C",
      500 -> "D",
      1000 -> "M"
    )
    var res = ""
    var reminder = x
    val level = Array(1000, 100, 10, 1)
    var lvlInd = 0

    while (reminder != 0 && lvlInd < level.length) {
      val thisLevel = level(lvlInd)
      val thisVal = reminder / thisLevel
//      println(s"Current Level: $thisLevel, count: $thisVal")
      if (thisVal == 9 && lvlInd - 1 >= 0) {
        val nextLevel = level(lvlInd - 1)
        res = res + romeMap(thisLevel) + romeMap(nextLevel)
      }
      else if (thisVal == 4 && lvlInd - 1 >= 0) {
//        val nextLevel = level(lvlInd)
        res = res + romeMap(thisLevel) + romeMap(thisLevel * 5)
      }
      else if (thisVal >= 5 && lvlInd - 1 >= 0) {
        res = res + romeMap(thisLevel * 5) + romeMap(thisLevel) * (thisVal - 5)
      }
      else
      {
        res = res + romeMap(thisLevel) * thisVal
      }
      reminder = reminder - thisVal * thisLevel
//      println(s"reminder: $reminder")
      lvlInd += 1
    }
    res
  }

  // -------------------------- *** Problem: Regular Expression Matching *** ----------------------

  def findFirstEq (s1: String, s2:String): Int = {
    val target = s1(0)
    var ind = 0
    val maxLen = s2.length
    while (s2(ind) != target && ind < maxLen){
      ind += 1
    }
    ind
  }

  def isMatchSimpleReg(str: String, pttrn: String): Boolean = {

    val strLen = str.length
    val pttrnLen = pttrn.length
    val tracker = Array.ofDim[Int](100, 100)

    def dynamic (i: Int, j: Int): Boolean = {
//      println (s"Entered Calculation of Dynamic for $i, $j")
      if (tracker(i)(j) == 0) {
        if (j == pttrnLen) { println("      reached max j"); tracker(i)(j) = 1; i == strLen }
        else{
          val thisMatch = i < strLen && ( str(i) == pttrn(j) || "" + pttrn(j) == "." )
          tracker(i)(j) = 1
          if (j + 1 < pttrnLen && ("" + pttrn(j + 1)) == "*"){
//            println("      reached pttrn(j+1) == *")
              dynamic(i, j + 2) || ( thisMatch && dynamic(i + 1, j) )
          }
          else{
//            println("      reached else")
            thisMatch && dynamic(i + 1, j + 1)
          }
        }
      }
      else{
        false
      }
    }

    dynamic(0, 0)
  }

  // -------------------------- *** Problem: Jewels and Stones *** ----------------------
  def findCharInString (base: String, target: String): Int = {
    target.filter(char => base.contains(char)).length
  }

  // -------------------------- *** Problem: Distinct Emails *** ----------------------
  def countDistinctEmailsByMapping (input: Seq[String]): Int = {
    val mapped = input.map(
      str => {
        val strArr = str.split("@")
        val localName = strArr
          .head
          .replace(".", "")
          .replaceAll("\\+[a-z]*[A-Z]*[0-9]*", "")
        localName + "@" + strArr.last
      }
    )
//    mapped.foreach(println)
    mapped.distinct.length
  }

  def countDistinctEmailsByIndx (input: Seq[String]): Int = {
    input.map ( str => {
      val arr = str.split("@")
      val localName = arr.head.replace(".", "")
      val domain = arr.last
      val cut = if (localName.indexOf("+") == -1) localName.length else localName.indexOf("+") + 1
      localName.substring(0, cut) + domain
    }
    )
      .distinct
      .length
  }

  // -------------------------- *** Problem: To Lowercase *** ----------------------
  def toLowerCase (str: String): String = {
    str.map(char => {if (char > 64 && char <= 90) (char + 32).toChar else char})
  }

  // -------------------------- *** Problem: Morse Coder *** ----------------------

  private val morseMap = Map(
    "a" -> ".-",
    "b" -> "-...",
    "c" -> "-.-.",
    "d" -> "-..",
    "e" -> ".",
    "f" -> "..-.",
    "g" -> "--.",
    "h" -> "....",
    "i" -> "..",
    "j" -> ".---",
    "k" -> "-.-",
    "l" -> ".-..",
    "m" -> "--",
    "n" -> "-.",
    "o" -> "---",
    "p" -> ".--.",
    "q" -> "--.-",
    "r" -> ".-.",
    "s" -> "...",
    "t" -> "-",
    "u" -> "..-",
    "v" -> "...-",
    "w" -> ".--",
    "x" -> "-..-",
    "y" -> "-.--",
    "z" -> "--.."
  )

  def morseCoder (strs: Seq[String]): Seq[String] = {
    strs.map(str => str.toLowerCase.map(char => morseMap(char.toString)).reduce(_ + _) )
  }

  def morseCount (strs: Seq[String]): Int = {
    morseCoder(strs).distinct.length
  }

  // -------------------------- *** Problem: Robot Return to Origin *** ----------------------
  def isStillOrgnByMap (str: String): Boolean = {
    val mvmtMap: Map[String, (Int, Int)] = Map (
      "U" -> (0, 1),
      "D" -> (0, -1),
      "L" -> (-1, 0),
      "R" -> (1, 0)
    )
    str.toCharArray.map(char => mvmtMap(char.toString)).foldLeft((0, 0))((x, y) => (x._1 + y._1, x._2 + y._2) ) == (0, 0)
  }

  def isStillOrgn (str: String): Boolean = {
    var x = 0
    var y = 0
    str.foreach( char => char match {
      case 'L' => x -= 1
      case 'R' => x += 1
      case 'D' => y -= 1
      case 'U' => y += 1
    }
    )
      x == 0 && y == 0
  }

  // -------------------------- *** Problem: First Unique Char in String *** ----------------------

  def firstUniqCharByMap(s: String): Int = {

    val charArr = s.toCharArray

    if (s.length == 0){ -1 }
    else {

      val res = charArr.groupBy(x => x) // Array[ (char, Array[Char]) ]
        .map(t => (charArr.indexOf(t._1), t._2.length)) //Array[ (Int, Int) ]
        .filter(_._2 == 1) //Array[ (Int, Int) ]

      if (res.isEmpty) -1 else res.keys.min
    }
  }

  def firstUniqChar(s: String): Int = {

    val counts = new Array[Int](26)
    var i = s.length - 1

    while (i >= 0) {
      counts(s(i) - 'a') += 1
      i = i - 1
    }

    var index = -1
    i = 0

    while (i < s.length && index < 0) {
      if (counts(s(i) - 'a') == 1) index = i
      else i = i + 1
    }

    index
  }

  // -------------------------- *** Problem: Next Smallest Char *** ----------------------
  def nextGreatestLetter(letters: Array[Char], target: Char): Char = {
    var low = 0
    var high = letters.length - 1

    while (low < high ){
      var mid = (high + low)/2
      if (letters(mid) <= target) {
        low = mid + 1
      }
      else {high = mid}
    }
    if (letters(low) <= target) letters(0) else letters(low)
  }

  // -------------------------- *** Problem: Longest Word *** ----------------------
  def canBuild (str: String, storage: Seq[String]): Boolean ={
    storage.filter( s => str.contains(s) && ( (s.length + 1) == str.length) ).length != 0
  }

  def longestWord(words: Array[String]): String = {
    val input = words.sortBy(s => s.length)
    input.foreach(println)
    val heads = input.filter(str => str.length == 1).distinct
    if (heads.length == 0) {
      ""
    }
    else {
      var bestLen = 1
      var bestStr = heads.min
      var ind = 0
      var storage = heads
      val maxLen = words.length

      while (ind < maxLen){
        val word = input(ind)
//        println(s"iter number: $ind, current word: $word")
        if ( canBuild(word, storage) ){
          storage = storage :+ word
          if (word.length > bestLen){
            bestStr = word
            bestLen = word.length
          }
          else if (word.length == bestLen && word < bestStr){
            bestStr = word
          }
        }
        ind += 1
      }

      bestStr
    }
  }

  def longestWordByTrie (words: Array[String]): String = {
    val root = new TrieNode()
    for (word <- words){
      root.addTrie(word)
    }

    findLongestContinuousPath(root, "")
  }

  // -------------------------- *** Problem: Add String Numbers *** ----------------------
  def addStrings(num1: String, num2: String): String = {
    val maxArr = if (num1.length > num2.length) num1.toCharArray else num2.toCharArray
    val minArr = if (num1.length > num2.length) num2.toCharArray else num1.toCharArray

    val minLen = minArr.length
    val maxLen = maxArr.length
    var i = maxLen - 1
    val diff = maxLen - minLen

    var carry = 0
    var res = ""

    while (i >= 0) {
//      println(s"Current Max Array Position: $i, Min Array Position: ${i - diff}")
      val thisValue = maxArr(i)- 48 + carry + (if (i - diff >= 0) (minArr(i - diff) - 48) else 0)
      carry = if (thisValue >= 10) 1 else 0
//      println(s"Current Carry: $carry, Add-on digit: ${thisValue%10}")
      res = (if (thisValue >= 10) (thisValue%10).toString else thisValue.toString) + res
      i -= 1
    }

    res = if (carry != 0) carry.toString + res else res
    res
  }

  // -------------------------- *** Problem: Back Space Editor *** ---------------------
  def afterBackSpace (str: String) = {
    val reg = "[a-zA-z]#".r
    var res = str
    while (!reg.findFirstIn(res).isEmpty) {
      res = reg.replaceAllIn(res, "")
    }
    res.replace("#", "")
  }

  def backEditorCompare (s: String, t: String) = {
    afterBackSpace(s) == afterBackSpace(t)
  }

  // -------------------------- *** Problem: Most Common Words *** ---------------------
  def mostCommonWord(paragraph: String, banned: Array[String]) = {
    paragraph
      .toLowerCase
      .replaceAll("[!\\?|'|,|;|\\.]", " ")
      .replace("  ", " ")
      .split(" ")
      .filter(str => !banned.contains(str))
      .groupBy(x => x)
      .toArray
      .map(t => (t._1, t._2.length))
      .sortBy(_._2)
      .last
      ._1
  }

  // -------------------------- *** Problem: Detect Capital *** ---------------------
  def detectCapitalUse(word: String): Boolean = {
    val arr = word.toCharArray
    val maxLen = arr.length

    if (arr.length == 1) {true}
    else if (arr.head >= 97 || (arr.head <= 90 && arr(1) >= 97)){
      var ind = 1
      var tf = true
      while (ind < maxLen && tf == true){
        tf = arr(ind) >= 97
        ind += 1
      }
      tf
    }
    else {
      var ind = 1
      var tf = true
      while (ind < maxLen && tf == true){
        tf = arr(ind) <= 90
        ind += 1
      }
      tf
    }
  }

  // -------------------------- *** Problem: Find the Difference *** ---------------------
  def findTheDifference(sArr: Array[Char], t: String): Char = {

    var ind = 0
    var memoryMap = scala.collection.mutable.Map[Char, Int]()
    val len = sArr.length

    while (ind < len){
      val lastCount = try {memoryMap(sArr(ind))} catch {case _: Exception => 0}
      memoryMap += ( sArr(ind) -> (lastCount + 1) )
      ind += 1
    }

    var trace = true
    val newLen = t.length
    ind = 0

    while (ind < newLen && trace == true){
      val thisChar = t(ind)
      trace = try {memoryMap(thisChar); true} catch {case _: Exception => false}
      if (trace == true) { memoryMap += ( thisChar -> (memoryMap(thisChar) - 1) ) }
      if (trace == true && memoryMap(thisChar) == -1){trace = false}
      ind += 1
    }
    t(ind - 1)
  }

  def findTheDifference(s: String, t: String): Char = {

    var charDiff: Int = t(s.length)

    s.indices.foreach( i => {
      charDiff -= s(i)
      charDiff += t(i)
    })

    charDiff.toChar
  }

  // -------------------------- *** Problem: Shortest Distance to a Character *** ---------------------
  def shortestToChar(s: String, c: Char): Array[Int] = {

    val arr = s.toCharArray
    val maxLen = arr.length
    var leftLoc = - 2 * maxLen
    var rightLoc = 2 * maxLen

    var ind = 0 //reverse ind = maxLen - 1 - ind

    val distance = Array.fill(maxLen)(maxLen)

    while (ind < maxLen){

      val reverseInd = maxLen - 1 - ind

      if (arr(ind) == c){leftLoc = ind; distance(ind) = 0}
      if (arr(reverseInd) == c){rightLoc = reverseInd; distance(reverseInd) = 0}

      println(s"Current Char Location: $ind, Current Left Location: $leftLoc, Current Right Location: $rightLoc" )

      distance(reverseInd) = math.min( rightLoc - reverseInd, distance(reverseInd) )
      distance(ind) = math.min( ind - leftLoc, distance(ind) )

      ind += 1
    }

    distance
  }

  // -------------------------- *** Problem: Shortest Completing Word *** ---------------------
  def charToPrime (s: Array[Char]): Array[Int] = {

    val primes = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103)
    var ind = 0
    var res = Array[Int]()

    while (ind < s.length){

      val thisChar = s(ind)

      res = if (thisChar >= 97 && thisChar <= 122) res :+ primes( thisChar - 'a' ) else res
      ind += 1
    }

    res
  }

  def strToPrimeProd (s: String): Long = {

    val primes = charToPrime(s.toLowerCase.toCharArray)
    var ind = 0
    var res: Long = 1

    while (ind < primes.length){
      res *= primes(ind)
      ind += 1
    }

    res
  }

  def shortestCompletingWord(licensePlate: String, words: Array[String]): String = {

    val ref = strToPrimeProd(licensePlate)
    var bestWord = " "
    var bestLen = Int.MaxValue
    var ind = 0

    while (ind < words.length){
      val thisWord = words(ind)
      val thisCharProd = strToPrimeProd(thisWord)
      if (thisCharProd % ref == 0 && thisWord.length < bestLen){
        bestWord = thisWord
        bestLen = thisWord.length
      }
      ind += 1
    }
    bestWord
  }


  // -------------------------- *** Problem: Fizz Buzz *** ---------------------
  def fizzBuzz(n: Int): List[String] = {

    var i = 1
    var list = collection.mutable.ArrayBuffer[String]()

    while (i <= n) {
      var s = ""
      if (i % 3 == 0)
        s += "Fizz"
      if (i % 5 == 0)
        s += "Buzz"
      if (s == "")
        s = i.toString
      list.append(s)
      i += 1
    }

    list.toList
  }


  // -------------------------- *** Problem: Letter Case Permutation *** ---------------------
  def isEnglishChar (char: Char): Boolean = {
    (char <= 122 && char >= 97) || (char <= 90 && char >= 65)
  }

  def letterCasePermutation(s: String): List[String] = {

    var res = List[String]()
    var ind = 0


    while (ind < s.length){
//      println(s"Current While Loop: Char -> ${s(ind)}")
//      println(s"Entering recurssion start at ${ind + 1}")
      var tempRes: List[String] = letterCasePermutation(s.substring(ind + 1, s.length))
//      println(s"Current temporary result: ${tempRes.foreach(println)}")
      if (tempRes.isEmpty){tempRes = List("")}
      if (isEnglishChar(s(ind))) {
          res = res ++ tempRes.map(str => s(ind).toUpper + str) ++ tempRes.map(str => s(ind).toLower + str)
      }
      else {
          res = res ++ tempRes.map(str => s(ind) + str)
      }
      ind += 1
    }
    res.filter(_.length == s.length)
  }

  def letterCasePermutationBackward(s: String): List[String] = {

    var res = List[String]("")
    var ind = s.length - 1

    if (s.length == 0) {
      List("")
    }
    else {
      while (ind >= 0) {

        if (isEnglishChar(s(ind))) {
          var subInd = 0
          var newRes = List[String]()
          while (subInd < res.length) {
            newRes = newRes :+ (s(ind).toUpper + res(subInd))
            newRes = newRes :+ (s(ind).toLower + res(subInd))
            subInd += 1
          }
          res = newRes
        }
        else {
          var subInd = 0
          var newRes = List[String]()
          while (subInd < res.length) {
            newRes = newRes :+ (s(ind) + res(subInd))
            subInd += 1
          }
          res = newRes
        }
        ind -= 1
      }
      res
    }
  }

  // -------------------------- *** Problem: IP to CIDR *** ---------------------
  def toNBitsBinary (x: Int, numBits: Int)= {
      var ind = 0
      while (Math.pow(2,ind) < x){
        ind += 1
      }
      val arr = Array.fill(numBits)(0)

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

  def binaryPlus1 (input: Array[Int]) = {
    var carry = 1
    var ind = 0
    val res = input
    while (carry != 0 && ind < input.length){
      if (res(ind) + carry == 2){
        res(ind) = 0
        carry = 1
      }
      else {
        res(ind) = res(ind) + carry
        carry = 0
      }
      ind += 1
    }
    res
  }

  def binaryRange (start: Int, length: Int, maxBits: Int): Array[Array[Int]] = {
    val binaryRep = toNBitsBinary(start, maxBits)
    var res = Array(binaryRep)
    var initial = start + 1

    while (initial < start + length){
      println(s"CurrentNumber: $initial")
      res = res :+ toNBitsBinary(initial, maxBits)
      println(s"Binary Version: ${toNBitsBinary(initial, maxBits).mkString(", ")}")
      initial += 1
    }
    res
  }

  def binaryArrayToString (binary: Array[Int]) = {
    binary.reverse.mkString
  }

  def binaryStrToNumber(s: String): Int = {
    val arr = s.toCharArray
    var ind = arr.length - 1
    var res = 0
    while (ind >= 0){
      res += math.pow(2, arr.length - 1 - ind).toInt * (arr(ind) - 48)
      ind -= 1
    }
    res
  }

  def binaryToIP(s: String): String = {
    binaryStrToNumber(s.substring(0, 8))  + "." +
      binaryStrToNumber(s.substring(9, 16)) + "." +
      binaryStrToNumber(s.substring(17, 24)) + "." +
      binaryStrToNumber(s.substring(24, 32))
  }

  // INCOMPLETE!!!

  def ipToCIDR(ip: String, n: Int): Unit = {

    val start = ip.split("\\.").map( str => binaryArrayToString(toNBitsBinary(str.toInt, 8)) ).reduce(_ + _)

    val maxPosToChange = {
      var ind = 0
      while (math.pow(2, ind) < n){
        ind += 1
      }
      ind + 1
    }

    if (maxPosToChange == n){
      val str = start.substring(0, start.length - maxPosToChange) + "0" * maxPosToChange
      binaryToIP(str)
    }
    else{
      var currentPosChange = maxPosToChange - 1
      val str = start.substring(0, start.length - currentPosChange) + "0" * currentPosChange
      str
    }
  }


  // -------------------------- *** Problem: Shortest Word Distance *** ---------------------
  def shortestDistance(words: Array[String], word1: String, word2: String): Int = {

    var ind = 0
    var pos1 = Int.MinValue/2
    var pos2 = Int.MaxValue/2
    var best = Int.MaxValue

    while (ind < words.length) {

      val currentWord = words(ind)

      if (currentWord == word1){
        pos1 = ind
        best = math.min(math.abs(pos2 - pos1), best)
      }
      else if (currentWord == word2){
        pos2 = ind
        best = math.min(math.abs(pos2 - pos1), best)
      }

      ind += 1
    }
    best
  }
  // -------------------------- *** Problem: License Key Formatting *** ---------------------
  def licenseKeyFormatting(S: String, K: Int): String = {
    val input = S.toUpperCase.toCharArray
    var ind = input.length - 1
    var count = 0
    var res: String = ""

    if (input.length == 0){
      res
    }
    else{

      while (ind >= 0){
        if (input(ind) != '-'){
//          println(s"Input ${input(ind)}, current result: $res, current count: $count, current remaining index: $ind")
          if (count % K == 0 && count != 0){
            res = "-" + res
          }
          res = input(ind) + res
          count += 1
//          println(s"Appended ${input(ind)}, current result: $res, current count: $count, current remaining index: $ind")
        }
        ind -= 1
      }
      res
    }
  }

  // -------------------------- *** Problem: License Key Formatting *** ---------------------

  def romanToInt(s: String): Int = {

    val romanToIntMap = scala.collection.mutable.Map[Char, Int](
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )

    val input: Array[Char] = s.toCharArray()

    if (input.length == 0){
      0
    }
    else if(input.length == 1){
      romanToIntMap(input(0))
    }
    else{

      var ind = 1
      var res = romanToIntMap(input(0))
      var lastVal = res
      var currentVal = res

      while (ind < input.length){
        println(s"Current Input: ${input(ind)}, last value: ${lastVal}, last res: $res")
        currentVal = romanToIntMap(input(ind))
        if (currentVal > lastVal){
          res += lastVal * (-2) + currentVal
        }
        else{
          res += currentVal
        }
        lastVal = currentVal
        println(s"updated res: $res, updated last value: $lastVal")
        ind += 1
      }
      res
    }
  }
  // -------------------------- *** Problem: Longest Common Prefix *** ---------------------
  def longestCommonPrefix(strs: Array[String]): String = {

    if (strs.isEmpty || strs(0).length == 0){
      ""
    }
    else {
      var prefix = strs(0)
      var ind = 1

      while (ind < strs.length) {

        val currentStr = strs(ind)
        var i = 0

        while (i < prefix.length && i < currentStr.length && currentStr(i) == prefix(i)) {
          i += 1
        }
        prefix = prefix.substring(0, i)

        ind += 1
      }
      prefix
    }
  }

  // -------------------------- *** Problem: Is Anagram *** ---------------------
  def isAnagram(s: String, t: String): Boolean = {
    if (s.length != t.length){
      return false
    }
    else{
      val record = new Array[Int](26)
      val sArr = s.toCharArray
      val tArr = t.toCharArray
      var i = 0

      while (i < sArr.length){
        record(sArr(i)- 'a') = record(sArr(i)- 'a') + 1
        i += 1
      }

      i = 0
      while (i < tArr.length){
        record(tArr(i)- 'a') = record(tArr(i)- 'a') -1
        i += 1
      }

      i = 0
      while(i< 26){
        if(record(i) != 0){
          return false
        }

        i += 1
      }
      true
    }
  }

}
