import DataStructures.Node
import DataStructures._

object LinkedListAlgorithms {

  // -------------------------- *** Problem: Add to List Format Numbers *** ----------------------

  def stringToList (x: String) = {

    val arr0 = x.split("\\+")
      .map(str => str
        .replace(" ", "")
        .replace(")", "")
        .replace("(", "")
      )

    if (arr0.length == 0) throw new Exception ("Invalid Entry")
    //    arr0.foreach(println)
    val arr1 = arr0(0).split("->").map(str => str.replace(" ", ""))
    val arr2 = arr0.last.split("->").map(str => str.replace(" ", ""))

    if ((arr1.length == 0) || (arr2.length == 0)) throw new Exception ("Invalid Entry")

    val intArr1 = arr1.map(x => try{(x.toInt)} catch {case e: Exception => 0})
    val intArr2 = arr2.map(x => try{(x.toInt)} catch {case e: Exception => 0})

    val list1 = new Node(intArr1.head, null)
    val list2 = new Node(intArr2.head, null)

    var ind1 = 1; var ind2 = 1

    //    intArr1.foreach(println); intArr2.foreach(println)

    while (ind1 < intArr1.length){
      list1 :+ (intArr1(ind1))
      //      println(list1.tail.data)
      ind1 += 1
    }

    while (ind2 < intArr2.length){
      list2 :+ (intArr2(ind2))
      //      println(list2.tail.data)
      ind2 += 1
    }

    (list1, list2)
  }

  def linkedListSum (list1: Node[Int], list2: Node[Int]) = {

    var current1 = list1
    var current2 = list2

    val resList = new Node(0, null)

    var ind1 = current1.next != null
    var ind2 = current2.next != null

    var carry = 0

    while (ind1 || ind2) {

      print(ind1, ind2)

      val data1 = if (!ind1) 0 else (current1.data)
      val data2 = if (!ind2) 0 else (current2.data)

      println(data1, data2)

      resList.addNode( (data1 + data2 + carry)%10 )

      if ((data1 + data2 + carry) >= 10) carry = 1 else carry = 0

      if (ind1) current1 = current1.next
      if (ind2) current2 = current2.next

      if (current1 == null) ind1 = false
      if (current2 == null) ind2 = false

    }

    resList.removeHead

  }


}
