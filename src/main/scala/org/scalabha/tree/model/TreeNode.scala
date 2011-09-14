package org.scalabha.model

import collection.mutable.HashMap

abstract class TreeNode {
  val name: String

  def compareStructure(other: TreeNode): Boolean

  def getMap(): HashMap[String, Set[List[String]]]
}

case class Value(name: String) extends TreeNode {
  def compareStructure(other: TreeNode): Boolean = {
    other.isInstanceOf[Value]
  }

  def getMap(): HashMap[String, Set[List[String]]] = HashMap[String, Set[List[String]]]()
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

  def getMap(): HashMap[String, Set[List[String]]] = {
    val result: HashMap[String, Set[List[String]]] = HashMap[String, Set[List[String]]](
      (name, Set((for (child <- children) yield child.name)))
    )

    for (child <- children) {
      val childMap = child.getMap()
      if (result.contains(child.name)){
        result(child.name) = result(child.name) ++ childMap(child.name)
      } else if (childMap.contains(child.name)) {
        result(child.name) = childMap(child.name)
      }
    }
    result

//    ++
//      (if (children.nonEmpty)
//        (for (child <- children) yield child.getMap()).reduce((a, b) => (a ++ b))
//      else
//        HashMap()
//        )
  }
}