package org.scalabha.model

abstract class TreeNode
case class Empty() extends TreeNode
case class Value(name: String) extends TreeNode
case class Node(name: String, children:List[TreeNode]) extends TreeNode