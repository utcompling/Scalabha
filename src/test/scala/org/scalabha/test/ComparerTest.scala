package org.scalabha.test

import org.scalabha.model._
import org.scalabha.tree.Comparer
import org.scalabha.tree.Comparer.{Fail, ComparisonResult, Success}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ComparerTest extends FlatSpec with ShouldMatchers {
  val tests = List[(String, String, ComparisonResult)](
    ("(a b)","(a b)", Success()),
    ("(a b)","(b b)", Fail("(a b)","(b b)")),
    ("(a b)","(a c)", Success()),
    ("(a b c)","(a b c)", Success()),
    ("(a b c d)","(a b c d)", Success()),
    ("(a (b c))","(a (b c))", Success()),
    ("(a (b c d) (e f g))","(a (b c d) (e f g))", Success()),
    ("(a b))","(a b))", Fail("(a b))","(a b))"))
  )

  for ((left, right, result) <- tests) {
    "\"%s\"".format(left) should "compare %s to \"%s\"".format(result, right) in {
      assert(Comparer(left,right) === result)
    }
  }
}