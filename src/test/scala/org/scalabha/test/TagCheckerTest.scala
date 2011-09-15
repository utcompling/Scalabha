package org.scalabha.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalabha.tree.TagChecker
import org.scalabha.model.{Value, Node, TreeNode}
import org.scalabha.tree.TagChecker.{Fail, Success, TagCheckResult}
import collection.immutable.HashMap

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
      Fail(Map("a" -> 1, "b" -> 1), Map("a" -> 1, "e" -> 1)))
  )

  for ((left, right, result) <- tests) {
    "TagChecker(%s,%s)".format(left,right) should "yield %s".format(result) in {
      assert(TagChecker(left,right) === result)
    }
  }

  "combineMaps" should "work" in {
    val l = HashMap(("a",1),("b",2))
    val r = HashMap(("a",3),("c",4))
    assert(TagChecker.combineMaps(l,r,(a:Int,b:Int)=>(a+b)) === HashMap(("a",4), ("b",2), ("c",4)))
  }
}