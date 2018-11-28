import DataStructures.LinkedList

object DataStructures {

  sealed trait LinkedList[A] {
    var data: A
    var next: LinkedList[A]
  }
//
//  case object NilNode extends LinkedList[Nothing] {
//    var data = throw new Exception ("You reached end of the linked list")
//    var next = throw new Exception ("You reached end of the linked list")
//  }

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

    def hasNext = ( this.next != null )

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

  case class Employee (var id: Int, var importance: Int, var subordinate: Array[Int]){

  }

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

  class NaiveListNode(var _x: Int = 0) {
    var next: NaiveListNode = null
    var x: Int = _x
  }

}
