import org.scalabha.model.{Node, Value}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TreeNodeTest extends FlatSpec with ShouldMatchers {
  "A Value" should "always compare true to a Value" in {
    assert(Value("1").compareStructure(Value("1")))
    assert(Value("1").compareStructure(Value("2")))
    assert(Value("1").compareStructure(Value("")))
  }

  it should "compare false to a Node" in {
    assert(!Value("1").compareStructure(Node("",List(Value("2")))))
  }

  "A Node" should "compare false to a Value" in {
    assert(!Node("",List(Value("2"))).compareStructure(Value("1")))
  }

  it should "compare true to a same-structured Node regardless of Value" in {
    val n1 = Node("a",List(Value("2")))
    val n2 = Node("a",List(Value("2")))
    assert(n1 compareStructure n2)
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("b", List(Value("5"), n2))
    assert(n3 compareStructure n4)
  }

  it should "compare false to a different-structured Node regardless of Value" in {
    val n1 = Node("b",List(Value("2")))
    val n2 = Node("a",List(Value("4")))
    assert(! n1.compareStructure(n2))
    val n3 = Node("b", List(Value("3"), n1))
    val n4 = Node("a", List(Value("5"), n2))
    assert(! n3.compareStructure(n4))
    assert(! n3.compareStructure(n1))
  }
}