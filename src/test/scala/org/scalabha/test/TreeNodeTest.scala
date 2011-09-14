package org.scalabha.test

import collection.immutable.HashMap
import org.scalabha.model.{TreeNode, Node, Value}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TreeNodeTest extends FlatSpec with ShouldMatchers {
  "A Value" should "always compare true to a Value" in {
    assert(Value("1").compareStructure(Value("1")))
    assert(Value("1").compareStructure(Value("2")))
    assert(Value("1").compareStructure(Value("")))
  }

  it should "compare false to a Node" in {
    assert(!Value("1").compareStructure(Node("", List(Value("2")))))
  }

  "A Node" should "compare false to a Value" in {
    assert(!Node("", List(Value("2"))).compareStructure(Value("1")))
  }

  it should "compare true to a same-structured Node regardless of Value" in {
    val n1 = Node("a", List(Value("2")))
    val n2 = Node("a", List(Value("2")))
    assert(n1 compareStructure n2)
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("b", List(Value("5"), n2))
    assert(n3 compareStructure n4)
  }

  it should "compare false to a different-structured Node regardless of Value" in {
    val n1 = Node("b", List(Value("2")))
    val n2 = Node("a", List(Value("4")))
    assert(!n1.compareStructure(n2))
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("a", List(Value("5"), n2))
    assert(!n3.compareStructure(n4))
    assert(!n3.compareStructure(n1))
  }

  val tests = List[(TreeNode, HashMap[String, Set[List[String]]])](
    (Node("a", List(Value("b"))), HashMap(("a", Set(List("b"))))),
    (Node("a", List(Value("b"), Value("c"))), HashMap(("a", Set(List("b", "c"))))),
    (Node("a", List(Node("b", List(Value("c"))))), HashMap(("a", Set(List("b"))), ("b", Set(List("c"))))),
    (Node("r", List(Node("a", List(Value("b"))), Node("a", List(Value("c"))), Node("a", List(Value("b"))))),
      HashMap(("a", Set(List("b"), List("c"))), ("r", Set(List("a", "a", "a"))))),
    (Node("a", List(Value("b"), Value("c"), Value("d"))), HashMap(("a", Set(List("b", "c", "d"))))),
    (Node("a", List(Node("b", List(Value("c"), Value("d"))), Node("e", List(Value("f"), Value("g"))))),
      HashMap(("a", Set(List("b", "e"))), ("b", Set(List("c", "d"))), ("e", Set(List("f", "g"))))),
    (Value("a"), HashMap()),
    (Node("a", List()), HashMap(("a", Set(List()))))
  )

  for ((tree, map) <- tests) {
    "%s.getMap()".format(tree) should "yield %s".format(map) in {
      assert(tree.getMap() === map)
    }
  }

  val tests2 = List[(TreeNode, Set[String])](
    (Node("a", List(Value("b"))), Set("a")),
    (Node("a", List(Value("b"), Value("c"))), Set("a")),
    (Node("a", List(Value("b"), Value("c"), Value("d"))), Set("a")),
    (Node("a", List(Node("b", List(Value("c"))))), Set("a", "b")),
    (Node("a", List(Node("b", List(Value("c"), Value("d"))), Node("e", List(Value("f"), Value("g"))))),
      Set("a", "b", "e")),
    (Value("a"), Set()),
    (Node("a", List()), Set("a"))
  )
  for ((tree, set) <- tests2) {
    "%s.getMap().keySet".format(tree) should "yield %s".format(set) in {
      assert(tree.getMap().keySet === set)
    }
  }
}