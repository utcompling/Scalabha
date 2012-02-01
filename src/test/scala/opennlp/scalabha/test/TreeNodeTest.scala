package opennlp.scalabha.test

import collection.immutable.HashMap
import opennlp.scalabha.model.{TreeNode, Node, Value}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TreeNodeTest extends FlatSpec with ShouldMatchers {
  val getTagChildren: (TreeNode) => List[TreeNode] =
    (node) => {
      node.getChildren.filter(!_.isToken)
    }
  val equal: (TreeNode, TreeNode) => Boolean =
    (a, b) => {
      (
        ((a.isToken && b.isToken) || (a.name == b.name))
          && (a.getChildren.length == b.getChildren.length)
          && ((a.getChildren zip b.getChildren).filter {
          case (ca, cb) => equal(ca, cb)
        }.length == a.getChildren.length)
        )
    }
  //TESTING THE TESTS ----------------------------------------------------------
  "A Value" should "always compare true to a Value" in {
    assert(equal(Value("1"), Value("1")))
    assert(equal(Value("1"), Value("2")))
    assert(equal(Value("1"), Value("")))
  }
  it should "compare false to a Node" in {
    assert(!equal(Value("1"), Node("", List(Value("2")))))
  }

  "A Node" should "compare false to a Value" in {
    assert(!equal(Node("", List(Value("2"))), Value("1")))
  }

  it should "compare true to a same-structured Node regardless of Value" in {
    val n1 = Node("a", List(Value("2")))
    val n2 = Node("a", List(Value("2")))
    assert(equal(n1, n2))
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("b", List(Value("5"), n2))
    assert(equal(n3, n4))
  }

  it should "compare false to a different-structured Node regardless of Value" in {
    val n1 = Node("b", List(Value("2")))
    val n2 = Node("a", List(Value("4")))
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("a", List(Value("5"), n2))
    assert(!equal(n1, n2))
    assert(!equal(n2, n3))
    assert(!equal(n3, n4))
  }

  //TESTING THE OBJECT --------------------------------------------------------

  "isTerminal" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))

    assert(!n0.isTerminal)
    assert(n1.isTerminal)
    assert(!n2.isTerminal)
  }

  "isToken" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))

    assert(n0.isToken)
    assert(!n1.isToken)
    assert(!n2.isToken)
  }

  "getTokenStrings" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))
    val n3 = Node("b", List(Node("a", List(Value("1"))),Node("b", List(Value("2")))))

    assert(n0.getTokenStrings === List("1"))
    assert(n1.getTokenStrings === List("1"))
    assert(n2.getTokenStrings === List("1"))
    assert(n3.getTokenStrings === List("1","2"))
  }

  "getTagStrings" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))
    val n3 = Node("b", List(Node("a", List(Value("1"))),Node("c", List(Value("2")))))

    assert(n0.getTagStrings === List())
    assert(n1.getTagStrings === List("a"))
    assert(n2.getTagStrings === List("b","a"))
    assert(n3.getTagStrings === List("b","a","c"))
  }

  "getCanonicalString" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))
    val n3 = Node("b", List(Node("a", List(Value("1"))),Node("c", List(Value("2")))))

    assert(n0.getCanonicalString === "1")
    assert(n1.getCanonicalString === "(a 1)")
    assert(n2.getCanonicalString === "(b (a 1) )")
    assert(n3.getCanonicalString === "(b (a 1) (c 2) )")
  }

  "getPrettyString" should "work" in {
    val n0 = Value("1")
    val n1 = Node("a", List(Value("1")))
    val n2 = Node("b", List(Node("a", List(Value("1")))))
    val n3 = Node("b", List(Node("a", List(Value("1"))),Node("c", List(Value("2")))))

    assert(n0.getPrettyString === "1")
    assert(n1.getPrettyString === "\n(a 1)")
    assert(n2.getPrettyString === "\n(b \n  (a 1) )")
    assert(n3.getPrettyString === "\n(b \n  (a 1) \n  (c 2) )")
  }
}
