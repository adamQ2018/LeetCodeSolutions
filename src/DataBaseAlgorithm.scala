import ArrayAlgorithms._
import LinkedListAlgorithms._
import StringAlgorithms._
import DataStructures._
import DataStructures.Node._
import TreeAlgorithm._
import MathAlgorithm._

object DataBaseAlgorithm {

  def getEmployeeImportance (employeeBook: Array[Employee], id: Int, departmentImp: Int): Int = {
    var ind = 0
    var accImp = departmentImp

    while ( ind < employeeBook.length && employeeBook(ind).id != id){
      ind += 1
    }

    val thisEmployee = employeeBook(ind)
    println(s"Start Achieve this Employee's info with id: ${thisEmployee.id}")
    val thisSubordinates = thisEmployee.subordinate

    accImp += thisEmployee.importance

    ind = 0

    while(ind < thisSubordinates.length){
      val thisSubordinateId = thisSubordinates(ind)
      println(s"Current Subordinates Id: $thisSubordinateId")
      accImp = getEmployeeImportance(employeeBook, thisSubordinateId, accImp)
      ind += 1
    }

    accImp
  }
}
