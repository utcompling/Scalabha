package opennlp.scalabha.test

import collection.immutable.HashMap
import opennlp.scalabha.model.{TreeNode, Node, Value}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TreeNodeTest extends FlatSpec with ShouldMatchers {
  val equal: (TreeNode, TreeNode) => Boolean =
    (a, b) => {
      (a.name == b.name)
      &&(a.getChildren.length == b.getChildren.length)
      &&((a.getChildren zip b.getChildren).filter(
        (ca: TreeNode, cb: TreeNode) => equal(ca, cb)
      ).length == a.getChildren.length)
    }
  
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

  for ((tree, map) <- tests) {
    "%s.getTagMap()".format(tree) should "yield %s".format(map) in {
      assert(tree.getTagMap() === map)
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
    "%s.getTagMap().keySet".format(tree) should "yield %s".format(set) in {
      assert(tree.getTagMap().keySet === set)
    }
  }

  val tests3 = List[(TreeNode, HashMap[String, Int])](
    (Node("a", List(Value("b"))), HashMap(("a", 1))),
    (Node("a", List(Value("b"), Value("c"))), HashMap(("a", 1))),
    (Node("a", List(Node("b", List(Value("c"))))), HashMap(("a", 1), ("b", 1))),
    (Node("r", List(Node("a", List(Value("b"))), Node("a", List(Value("c"))), Node("a", List(Value("b"))))),
      HashMap(("a", 3), ("r", 1))),
    (Node("a", List(Value("b"), Value("c"), Value("d"))), HashMap(("a", 1))),
    (Node("a", List(Node("b", List(Value("c"), Value("d"))), Node("e", List(Value("f"), Value("g"))))),
      HashMap(("a", 1), ("b", 1), ("e", 1))),
    (Value("a"), HashMap()),
    (Node("a", List()), HashMap(("a", 1)))
  )

  for ((tree, map) <- tests3) {
    "%s.getTagCounts()".format(tree) should "yield %s".format(map) in {
      assert(tree.getTagCounts() === map)
    }
  }
}
