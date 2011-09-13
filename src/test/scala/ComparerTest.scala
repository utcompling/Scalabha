import org.scalabha.model._
import org.scalabha.tree.Comparer
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ComparerTest extends FlatSpec with ShouldMatchers {
  val tests = List[(String, String, Boolean)](
    ("(a b)","(a b)", true),
    ("(a b)","(b b)", false),
    ("(a b)","(a c)", true),
    ("(a b c)","(a b c)", true),
    ("(a b c d)","(a b c d)", true),
    ("(a (b c))","(a (b c))", true),
    ("(a (b c d) (e f g))","(a (b c d) (e f g))", true),
    ("(a b))","(a b))", false)
  )

  for ((left, right, result) <- tests) {
    "\"%s\"".format(left) should "compare %s to \"%s\"".format(result, right) in {
      assert(Comparer(left,right) === result)
    }
  }
}