import DataStructures.LinkedList

import scala.reflect.ClassTag
import scala.util.Random

object DataStructures {

  sealed trait LinkedList[A] {
    var data: A
    var next: LinkedList[A]
  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Linked List Structure -------------
  // ----------------------------- **************************** ---------------------
  class Node[A](var data: A, var next: Node[A]){

    class NilNode extends Node[A](data, next){
      data = throw new Exception("You reached end of the linked list")
      next = throw new Exception("You reached end of the linked list")
    }

    def addNode(x: A) = {
      this.tail.next = new Node(x, null)
      this
    }

    def replace(x : A): DataStructures.Node[A] ={
      this.data = x
      this
    }

    def insertNode(x: A) ={
      val that = new Node (x, null)
      val prevNext = this.next
      this.next = that
      that.next = prevNext
    }

    def head = this

    def tail = {
      var current = this
      while (current.next != null){
        current = current.next
      }
      current
    }

    def hasNext = this.next != null

    def :+ (x: A) = addNode (x)

    def append (x: Node[A]) = {
      val tl = this.tail
      tl.next = x
      this
    }

    def prepend (x: Node[A]) = {
      x.tail.next = new Node(this.data, this.next)
      this.next = x.next
      this.data = x.data
      this
    }

    def prependNode(x: A) ={
      val that = new Node (this.data, this.next)

      this.data = x
      this.next = that
      this
    }

    def +:(x: A) = prependNode(x)

    def removeHead = {
      val next = this.next
      this.data = next.data
      this.next = next.next
      this
    }

    def removeNext = {
      val next = this.next
      this.next = next.next
      this
    }

    override def toString: String = {

      var current = this

      var res: String = ""

      while (current != null) {
        res = res + current.data.toString
        current = current.next
      }

      res
    }

    def toStringList: String = {

      var current = this

      var res: String = current.data.toString

      while (current.next != null) {

        current = current.next

        res = res + " -> " + current.data.toString

      }

      res
    }

  }

  object Node {

    def apply (x: Int, next: Node[Int]): Node[Int] = new Node[Int](x, next)

    def apply (x: Int): Node[Int] = new Node(x, null)

    def apply (x: String, next: Node[String]): Node[String] = new Node(x, next)

    def apply (x: Double, next: Node[Double]): Node[Double] = new Node[Double](x.toDouble, next)

//    def apply (x: Long, next: Node[Long]): Node[Long] = new Node[Long](x.toLong, next)
  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Binary Tree Structure -------------
  // ----------------------------- **************************** ---------------------
  class SimpleBinaryTree[A] (var data: A,  var left: SimpleBinaryTree[A], var right: SimpleBinaryTree[A]){

    class NilTree extends SimpleBinaryTree[A](data, left, right){
      data = throw new Exception("You reached end of the Tree")
      left = throw new Exception("You reached end of the Tree")
      right = throw new Exception("You reached end of the Tree")
    }

    def addLeftNode(x: A) = {
      this.left = new SimpleBinaryTree(x, null, null)
      this
    }

    def addRightNode(x: A)  = {
      this.left = new SimpleBinaryTree(x, null, null)
      this
    }

    def head = this

  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Trie Structure --------------------
  // ----------------------------- **************************** ---------------------
  class TrieNode (var thisChar: Char, var child: Array[TrieNode], var isWord: Boolean, var word: String) {

    def this(value: Char) {
      this(value, new Array[TrieNode](26), false, null)
    }

    def this(){
      this(' ')
    }

    def addChar (trie: TrieNode, char: Char) = {
      if (trie.child(char - 'a') == null) trie.child(char - 'a') = new TrieNode(char)
    }

    def addTrie (word: String) = {
      val charArr = word.toCharArray
      var ind = 0
      var current = this
      val wordLen = word.length

      while (ind < wordLen) {
        val char = charArr(ind)
        if (current.child(char - 'a') == null) {
          addChar(current, char)
        }
        current = current.child(char - 'a')
        ind += 1
      }
      current.word = word
    }

  }

  // ---------------------------------------- **************************** ----------------------------------
  // --------------------------------- Structure: Simple Employee Info Structure ----------------------------
  // ---------------------------------------------- Problem Specific ----------------------------------------
  case class Employee (var id: Int, var importance: Int, var subordinate: Array[Int])

  class Stack[A] {

    var elements: List[A] = Nil

    def push(x: A) = {
      this.elements = x :: this.elements
    }

    def peek = {
      this.elements.head
    }

    def pop (): A = {
      val current = this.peek
      this.elements = this.elements.tail
      current
    }

    def isEmpty = this.elements.isEmpty

  }

  // ---------------------------------------- **************************** ----------------------------------
  // --------------------------------- Structure: Simple Linked List Structure ------------------------------
  // ---------------------------------------------- Problem Specific ----------------------------------------
  case class NaiveListNode(var _x: Int = 0) {
    var next: NaiveListNode = null
    var x: Int = _x
  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Min Stack -------------------------
  // ----------------------------- **************************** ---------------------
  class MinStack(var elements: Array[Int], var min: Int){

    def push(x: Int): Unit = {
      val orgnlElements: Array[Int] = this.elements
      this.elements = orgnlElements :+ x
      if (this.min > x) {this.min = x}
    }

    def pop = {
      val orgnlElements: Array[Int] = this.elements
      this.elements = orgnlElements.dropRight(1)
      if (this.elements.isEmpty){
        this.min = Int.MaxValue
      }
      else{
        this.min = this.elements.min
      }
    }

    def top: Unit = {
      this.elements.last
    }

    def getMin: Int = {
      this.min
    }

    def this(min: Int){
      this(Array(min), min)
    }

    def this(){
      this(Array[Int](), Int.MaxValue)
    }

  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Max Stack -------------------------
  // ----------------------------- **************************** ---------------------
  class MaxStack(var elements: Array[Int], var max: Int){

    var maxLoc = -1

    def push(x: Int): Unit = {
      this.elements = this.elements :+ x
      if (x >= max){
        this.max = x
        maxLoc = this.elements.length - 1
      }
    }

    def pop: Int = {
      if (this.elements.last == max){
        popMax
      }
      else{
        val temp = this.top
        this.elements = this.elements.dropRight(1)
        temp
      }
    }

    def top(): Int = {
      this.elements.last
    }

    def peekMax(): Int = {
      this.max
    }

    def popMax(): Int = {
      val temp = this.max

      if(this.elements.length == 1){
        this.elements = this.elements.dropRight(1)
        this.max = Int.MinValue
        this.maxLoc = -1
      }
      else{

        var i = 0
        this.max = Int.MinValue
        var tempMaxLoc = -1

        while (i < maxLoc){
          if (this.elements(i) >= this.max){
            this.max = this.elements(i)
            tempMaxLoc = i
          }
          i += 1
        }// now i should be max loc

        while (i < this.elements.length - 1){
          this.elements(i) = this.elements(i+1)
          if(this.elements(i+1) >= this.max){
            this.max = this.elements(i+1)
            tempMaxLoc = i
          }
          i += 1
        }
        this.elements = this.elements.dropRight(1)
        maxLoc = tempMaxLoc
      }
      temp
    }


    def this(max: Int){
      this(Array(max), max)
    }

    def this(){
      this(Array[Int](), Int.MinValue)
    }

  }

  // ----------------------------- **************************** ---------------------
  // --------------------------------- Structure: Queue     -------------------------
  // ----------------------------- **************************** ---------------------
  class Queue[A: ClassTag](val size: Int) {

    var currentLoc = 0
    var currentSize = 0
    var elements = new Array[A](size)

    def add(x: A): Unit = {
      elements(currentLoc) = x
      currentLoc = if ((currentLoc + 1) > size - 1) 0 else currentLoc + 1
      currentSize = if (currentSize + 1 > size) size else currentSize + 1
    }

    def += (x: A) = {
      add(x)
    }


    def swap(loc1: Int, loc2: Int): Unit ={
      val temp = elements(loc1)
      elements(loc1) = elements(loc2)
      elements(loc2) = temp
    }

    def last = {
      elements.last
    }

    def head = {
      elements.head
    }

    def merge (that: Queue[A]) = {
      val res = new Queue[A](that.size + this.size)
      res.elements = that.elements ++ this.elements
      res.currentSize = that.currentSize + this.currentSize
      res.currentLoc = if (res.currentSize == res.size) 0 else res.currentSize - 1
      res
    }

    def this() = {
      this(100)
    }
  }

  // ----------------------------- **************************** ---------------------
  // ------------------------------ Structure: Moving Average -------------------------
  // ------------------------------ **** By Using Queue ***** ---------------------
  class MovingAverage(size: Int){
    var elements = new Queue[Int](size)

    def next(x: Int) = {
      elements += x
      elements.elements.sum/elements.currentSize.toDouble
    }
  }
  // ----------------------------- **************************** ---------------------
  // ------------------------------ Structure: Moving Average -------------------------
  // ------------------------------ **** By Using Queue ***** ---------------------
  class StackByQueue[A: ClassTag] {

    var elements = scala.collection.mutable.Queue[A]()

    def push(x: A) {
      val helper = scala.collection.mutable.Queue[A]()
      helper += x
//       println(s"Pushed $x onto helper")
      while(! elements.isEmpty){
        val ele = this.top()
        helper += ele
//         println(s"Pushed $ele onto helper")
        this.pop
        // println(s"Popped $ele from elements, current elements length: ${elements.length}")
      }
      this.elements = helper
      // println(s"Assign elements to the value of helper")
    }

    def pop(): A = {
      elements.dequeue
    }

    def top(): A = {
      elements.front
    }

    def empty(): Boolean = {
      elements.isEmpty
    }
  }

  // Random Set
  class RandomizedSet {

    /** Initialize your data structure here. */
    var record = Array[Int]()
    var locs = scala.collection.mutable.HashMap[Int, Int]()
    var size: Int = 0

    /** Inserts a value to the set. Returns true if the set did not already contain the specified element. */
    def insert(x: Int): Boolean = {
      if (!locs.contains(x)){
        locs += (x -> size)
        record = record :+ x
        size += 1
        true
      }
      else{
        false
      }
    }

    /** Removes a value from the set. Returns true if the set contained the specified element. */
    def remove(x: Int): Boolean = {
      if (locs.contains(x)){
        val loc = locs(x) //get the location of this one
        val lastValue = record.last
        locs += (lastValue -> loc) // assign the last value to the empty location
        locs -= x //remove the value key
        record(loc) = lastValue //reassign last value to the current location
        record = record.dropRight(1)
        size -= 1
        true
      }
      else {
        false
      }
    }

    /** Get a random element from the set. */
    def getRandom(): Int = {
      val rand = new Random()
      val value = rand.nextInt(size)
      record(value)
    }
  }

}
