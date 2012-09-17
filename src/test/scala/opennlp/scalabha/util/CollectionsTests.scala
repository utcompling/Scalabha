package opennlp.scalabha.util

import org.junit.Assert._
import org.junit.Test
import scala.collection.mutable.Buffer
import opennlp.scalabha.util.Collections._
import opennlp.scalabha.util.CollectionUtil._

class CollectionsTests {

  @Test
  def test_MemoMap() {

    val calculated = Buffer[String]()

    val m = new MemoMap(Map("a" -> 1, "b" -> 2, "c" -> 3),
      (key: String) => {
        calculated += key
        Map("d" -> 4, "e" -> 5, "f" -> 6)(key)
      })

    assertEqualsSameElements(Buffer(), calculated)
    assertEquals(1, m("a"))
    assertEqualsSameElements(Buffer(), calculated)
    assertEquals(4, m("d"))
    assertEqualsSameElements(Buffer("d"), calculated)
    assertEquals(5, m("e"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(2, m("b"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)

    assertEquals(1, m("a"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(4, m("d"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(5, m("e"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
    assertEquals(2, m("b"))
    assertEqualsSameElements(Buffer("d", "e"), calculated)
  }

  def assertEqualsSameElements[T: Ordering](expected: Seq[T], actual: Seq[T]) {
    assertEquals("%s vs %s: DIFFERENCE: (%s)".format(expected, actual, (expected ++ actual).toSet -- (expected.toSet & actual.toSet)), expected.sorted.size, actual.sorted.size)
    for ((e, a) <- expected.sorted zipSafe actual.sorted)
      assertEquals(e, a)
  }

}
