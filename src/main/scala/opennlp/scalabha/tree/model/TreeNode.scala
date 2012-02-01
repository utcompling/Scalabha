package opennlp.scalabha.model

import collection.mutable.HashMap

abstract class TreeNode {
  protected[model] val _indent = "  "

  protected[model] def _prettyPrintStringPrefixed(prefix: String): String

  val name: String

  def isHead: Boolean

  def isTerminal: Boolean

  def isToken: Boolean

  def getChildren: List[TreeNode]

  def getTokenStrings: List[String]

  def getTagStrings: List[String]

  def getCanonicalString: String

  def getPrettyString: String

}

case class Value(name: String) extends TreeNode {
  // Values are heads by definition.
  // This isn't really meaningful, but since leaf Nodes are 
  // defined to have exactly one child, it makes validity checks easy
  def isHead: Boolean = true

  // terminals are interior nodes that contain Values as children. Values themselves are not terminals
  def isTerminal = false

  def isToken = true

  def getChildren = Nil

  def getTokenStrings: List[String] = List(name)

  def getTagStrings = Nil // tokens are not tags, obviously

  def getCanonicalString: String = name

  def _prettyPrintStringPrefixed(prefix: String): String = name

  def getPrettyString: String = name
}

case class Node(name: String, children: List[TreeNode]) extends TreeNode {
  // if we decide to strip the head marking from the name, we can change this later to be set on
  // object creation.
  def isHead: Boolean = name.endsWith("-H")

  // I'm defining a terminal as a node with only Token children.
  // Another valid definition might be a node with _any_ Token child.
  // Since the conventions we have adopted specify that terminals should
  // have exactly one child and that child should be a Token, there isn't
  // currently a difference.
  def isTerminal: Boolean = {
    children.filter(child => child.isToken) == children
  }

  def isToken = false

  def getChildren = children

  def getTokenStrings: List[String] = {
    (for (child <- children) yield {
      child.getTokenStrings
    }).toList.flatten
  }

  def getTagStrings = {
    name :: children.flatMap(child=>child.getTagStrings)
  }

  def getCanonicalString: String = "(%s %s)".format(
    name,
    (for (child <- children) yield child.getCanonicalString).mkString(" ")
      + (if (children != Nil && children.last.isInstanceOf[Node]) " " else "")
  )

  def _prettyPrintStringPrefixed(prefix: String): String = "%s(%s %s)".format(
    prefix,
    name,
    (for (child <- children) yield child._prettyPrintStringPrefixed(prefix + _indent)).mkString(" ")
      + (if (children != Nil && children.last.isInstanceOf[Node]) " " else "")
  )

  def getPrettyString: String = _prettyPrintStringPrefixed("\n")
}
