package org.scalabha.model

abstract class TreeNode {
  def compareStructure(other: TreeNode): Boolean
}

case class Value(name: String) extends TreeNode {
  def compareStructure(other: TreeNode): Boolean = {
    other.isInstanceOf[Value]
  }
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
}