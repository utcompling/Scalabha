package opennlp.scalabha.test

import opennlp.scalabha.model._
import opennlp.scalabha.tree.Parser
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParserTest extends FlatSpec with ShouldMatchers {
  val tests = List[(String, Option[TreeNode])](
    ("(a b)", Some(Node("a", List(Value("b"))))),
    ("(a b c)", Some(Node("a", List(Value("b"), Value("c"))))),
    ("(a b c d)", Some(Node("a", List(Value("b"), Value("c"), Value("d"))))),
    ("(a (b c))", Some(Node("a", List(Node("b", List(Value("c"))))))),
    ("(a (b c d) (e f g))", Some(Node("a", List(Node("b", List(Value("c"), Value("d"))), Node("e", List(Value("f"), Value("g"))))))),
    ("(a b))", None),
    ("(a b", None),
    ("", None),
    ("a", Some(Value("a"))),
    ("(a)", Some(Node("a",List())))
  )

  for ((string, result) <- tests) {
    "\"" + string + "\"" should "parse to " + result.toString in {
      assert(Parser(0, string) === result)
    }
  }

}
