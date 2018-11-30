import ArrayAlgorithms._
import LinkedListAlgorithms._
import StringAlgorithms._
import DataStructures._
import DataStructures.Node._
import TreeAlgorithm.findTreePathSum


object TreeAlgorithm {

  def mergeIntTree (thisTree: SimpleBinaryTree[Int], thatTree: SimpleBinaryTree[Int]): SimpleBinaryTree[Int] = {
    if (thatTree == null) { thisTree}
    else if (thisTree == null){ thatTree}
    else{
      thisTree.data = thisTree.data + thatTree.data
      thisTree.left = mergeIntTree(thisTree.left, thatTree.left)
      thisTree.right = mergeIntTree(thisTree.right, thatTree.right)
      thisTree
    }
  }

  // -------------------------- *** Problem: Longest Words can Built from Others *** ----------------------

  def addChar (trie: TrieNode, char: Char) = {
    if (trie.child(char - 'a') == null) trie.child(char - 'a') = new TrieNode(char)
  }

  def addTrie (trie: TrieNode, word: String) = {
    val charArr = word.toCharArray
    var ind = 0
    var current = trie
    val wordLen = word.length

    while (ind < wordLen) {
      val char = charArr(ind)
      if (current.child(char - 'a') == null) {
        addChar(current, char)
      }
      current = current.child(char - 'a')
      ind += 1
    }
  }

  def buildTrie(word: String) = {
    val root = new TrieNode()
    addTrie(root, word)
    root
  }


  def findDeepestNode (node: TrieNode, ans: String): String = {

    var i = 0
    val bestLen = ans.length
    var bestStr = ans

    while (i < 26){

      val thisNode = node.child(i)

      if (thisNode != null){
        if (thisNode.word != null){
          if ( thisNode.word.length > bestLen || (thisNode.word.length == bestLen && thisNode.word < bestStr) ){
            bestStr = thisNode.word
          }
        }
        bestStr = findDeepestNode(thisNode, bestStr)
      }
      i += 1
    }
    bestStr
  }

  def findLongestContinuousPath (node: TrieNode, ans: String): String = {

    var i = 0
    val bestLen = ans.length
    var bestStr = ans

    while (i < 26){

      val thisNode = node.child(i)
      var currentPathBest = bestStr

      if (thisNode != null && thisNode.word != null){
          if ( (thisNode.word.length > bestLen)  || (thisNode.word < currentPathBest && thisNode.word.length == currentPathBest.length)){
            currentPathBest = thisNode.word
        }
        currentPathBest = findLongestContinuousPath(thisNode, currentPathBest)
      }

      bestStr = if ( (currentPathBest < bestStr && currentPathBest.length == bestStr.length) || currentPathBest.length > bestStr.length) currentPathBest else bestStr
      i += 1

    }
    bestStr
  }

  // -------------------------- *** Problem: Binary Tree Diameter *** ----------------------

  def diameterOfBinaryTree(root: SimpleBinaryTree[Int]): Int = {
    var diameter = 0

    def findTreePathSum (node: SimpleBinaryTree[Int]): Int = {
      if (node != null){
        val leftPathLen = findTreePathSum(node.left)
        val rightPathLen = findTreePathSum(node.right)
        diameter = math.max(diameter, leftPathLen + rightPathLen)
        math.max(leftPathLen, rightPathLen) + 1
      } else{
        0
      }
    }

    findTreePathSum(root)
    diameter
  }

}
