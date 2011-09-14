package org.scalabha.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalabha.tree.TagChecker
import org.scalabha.model.{Value, Node, TreeNode}
import org.scalabha.tree.TagChecker.{Fail, Success, TagCheckResult}

class TagCheckerTest extends FlatSpec with ShouldMatchers {
  val tests = List[(TreeNode, TreeNode, TagCheckResult)](
    (Node("a", List(Value("b"))),
      Node("a", List(Value("b"))),
      Success()),
    (Node("a", List(Value("b"), Value("c"))),
      Node("a", List(Value("b"), Value("c"))),
      Success()),
    (Node("a", List(Value("b"), Value("c"))),
      Node("a", List(Value("e"), Value("f"))),
      Success()),
    (Node("a", List(Node("b", List(Value("c"))))),
      Node("a", List(Node("e", List(Value("f"))))),
      Fail(Set("a", "b"), Set("a", "e")))
  )

  for ((left, right, result) <- tests) {
    "TagChecker(%s,%s)".format(left,right) should "yield %s".format(result) in {
      assert(TagChecker(left,right) === result)
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
      assert(TagChecker(tree) === set)
    }
  }
}