package org.scalabha.model

import collection.mutable.HashMap

abstract class TreeNode {
  val name: String

  def compareStructure(other: TreeNode): Boolean

  def getTagMap(): HashMap[String, HashMap[List[String], Int]]

  def getTokens(): List[String]

  def getCanonicalString(): String

  def getTagCounts(): HashMap[String, Int] = {
    val result = HashMap[String, Int]()
    for ((nodeName, innerMap) <- getTagMap()) {
      for ((list, count) <- innerMap) {
        if (result.contains(nodeName))
          result(nodeName) += count
        else
          result(nodeName) = count
      }
    }
    result
  }
}

case class Value(name: String) extends TreeNode {
  def compareStructure(other: TreeNode): Boolean = {
    other.isInstanceOf[Value]
  }

  def getTagMap(): HashMap[String, HashMap[List[String], Int]] = HashMap[String, HashMap[List[String], Int]]()

  def getTokens(): List[String] = List(name)

  def getCanonicalString(): String = name

}

case class Node(name: String, children: List[TreeNode]) extends TreeNode {
  def compareStructure(other: TreeNode): Boolean = {
    var result = other.isInstanceOf[Node]
    if (result) {
      val otherNode: Node = other.asInstanceOf[Node]
      if (result) result &= (name == otherNode.name) && (children.length == otherNode.children.length)
      if (result)
        for ((t, o) <- (children zip otherNode.children)) {
          if (result) result &= t.compareStructure(o)
        }
    }
    result
  }

  def getTokens(): List[String] = {
    (for (child <- children) yield {
      child.getTokens()
    }).toList.flatten
  }

  def getTagMap(): HashMap[String, HashMap[List[String], Int]] = {
    val result: HashMap[String, HashMap[List[String], Int]] = HashMap[String, HashMap[List[String], Int]](
      (name, HashMap((for (child <- children) yield child.name) -> 1))
    )

    for (child <- children) {
      val childMap = child.getTagMap()
      for ((nodeName, innerMap) <- childMap) {
        for ((list, count) <- innerMap) {
          if (result.contains(nodeName)) {
            if (result(nodeName).contains(list)) {
              result(nodeName)(list) += count
            } else {
              result(nodeName)(list) = count
            }
          } else {
            result(nodeName) = HashMap(list -> count)
          }
        }
      }
    }
    result
  }

  def getCanonicalString(): String = " (%s %s) ".format(name, (for (child <- children) yield child.getCanonicalString()).mkString(" "))
}