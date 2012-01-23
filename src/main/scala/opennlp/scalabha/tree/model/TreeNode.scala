package opennlp.scalabha.model

import collection.mutable.HashMap

abstract class TreeNode {
  val name: String
  
  def isHead(): Boolean

  def compareStructure(other: TreeNode): Boolean

  def getTagMap(): HashMap[String, HashMap[List[String], Int]]

  def getTokens(): List[String]

  def getCanonicalString(): String

  val _indent = "  "
  def _prettyPrintStringPrefixed(prefix:String): String
  def prettyPrintString(): String

  def getHeight(): Int

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
  
  // Values are heads by definition.
  // This isn't really meaningful, but since leaf Nodes are 
  // defined to have exactly one child, it makes validity checks easy
  def isHead(): Boolean = true

  def getTagMap(): HashMap[String, HashMap[List[String], Int]] = HashMap[String, HashMap[List[String], Int]]()

  def getTokens(): List[String] = List(name)

  def getCanonicalString(): String = name

  def prettyPrintString(): String = name
  def _prettyPrintStringPrefixed(prefix:String): String = name

  def getHeight(): Int = 0
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

  // if we decide to strip the head marking from the name, we can change this later to be set on
  // object creation.
  def isHead(): Boolean = name.endsWith("-H")

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

  def getCanonicalString(): String = "(%s %s)".format(
    name,
    (for (child <- children) yield child.getCanonicalString()).mkString(" ")
      + (if (children != Nil && children.last.isInstanceOf[Node]) " " else "")
  )

  def _prettyPrintStringPrefixed(prefix:String): String = "%s(%s %s)".format(
    prefix,
    name,
    (for (child <- children) yield child._prettyPrintStringPrefixed(prefix + _indent)).mkString(" ")
      + (if (children != Nil && children.last.isInstanceOf[Node]) " " else "")
  )
  
  def prettyPrintString(): String = _prettyPrintStringPrefixed("\n")

  def maxChildHeight(children: List[TreeNode]): Int = {
    if (children.length == 0) {
      0
    } else {
      val c :: cs = children
      math.max(c.getHeight(), maxChildHeight(cs))
    }
  }

  def getHeight(): Int = {
    1 + maxChildHeight(children)
  }
}
