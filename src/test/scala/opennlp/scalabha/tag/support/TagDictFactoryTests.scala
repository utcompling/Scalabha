package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test

import opennlp.scalabha.tag._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.test.TestUtils._
import opennlp.scalabha.util.CollectionUtil._

class TagDictFactoryTests {

  @Test
  def test_SimpleWeightedTagDictFactory_passthroughTransformer() {
    val td: WeightedTagDict[String, Symbol] =
      new SimpleWeightedTagDictFactory[String, Symbol](
        PassthroughCondCountsTransformer())
        .make(Seq(Vector(
          ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D),
          ("dog", 'N), ("dog", 'N), ("dog", 'N),
          ("walks", 'V), ("walks", 'V), ("walks", 'V), ("walks", 'N))))

    // TagDict methods

    assertEquals(Set('D, 'N, 'V), td.defaultSet)

    assertEquals(Some(Set('D)), td.doGetSet("the"))
    assertEquals(Some(Set('N)), td.doGetSet("dog"))
    assertEquals(Some(Set('V, 'N)), td.doGetSet("walks"))
    assertEquals(None, td.doGetSet("aardvark"))

    val setIterator: Iterator[(String, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector("dog" -> Set('N), "the" -> Set('D), "walks" -> Set('V, 'N)), setIteratorSorted)

    assertEquals(Set('D), td.set("the"))
    assertEquals(Set('N), td.set("dog"))
    assertEquals(Set('V, 'N), td.set("walks"))
    assertEquals(Set('D, 'N, 'V), td.set("aardvark"))

    assertEquals(true, td.contains("the"))
    assertEquals(true, td.contains("dog"))
    assertEquals(true, td.contains("walks"))
    assertEquals(false, td.contains("aardvark"))

    assertEquals(true, td.contains("the", 'D))
    assertEquals(false, td.contains("the", 'N))
    assertEquals(false, td.contains("the", 'V))
    assertEquals(false, td.contains("the", 'Z))

    assertEquals(false, td.contains("dog", 'D))
    assertEquals(true, td.contains("dog", 'N))
    assertEquals(false, td.contains("dog", 'V))
    assertEquals(false, td.contains("dog", 'Z))

    assertEquals(false, td.contains("walks", 'D))
    assertEquals(true, td.contains("walks", 'N))
    assertEquals(true, td.contains("walks", 'V))
    assertEquals(false, td.contains("walks", 'Z))

    assertEquals(false, td.contains("aardvark", 'D))
    assertEquals(false, td.contains("aardvark", 'N))
    assertEquals(false, td.contains("aardvark", 'V))
    assertEquals(false, td.contains("aardvark", 'Z))

    assertEquals(Set("the", "dog", "walks"), td.symbols)

    assertEquals(Set('D, 'N, 'V), td.allTags)

    // WeightedTagDict methods

    assertEqualsSmart(Map('D -> LogNum(5 / 12.), 'N -> LogNum(4 / 12.), 'V -> LogNum(3 / 12.)), td.default)

    assertEqualsSmart(Some(Map('D -> LogNum(5 / 5.))), td.doGetMap("the"))
    assertEqualsSmart(Some(Map('N -> LogNum(3 / 3.))), td.doGetMap("dog"))
    assertEqualsSmart(Some(Map('V -> LogNum(3 / 4.), 'N -> LogNum(1 / 4.))), td.doGetMap("walks"))
    assertEqualsSmart(None, td.doGetMap("aardvark"))

    val iterator: Iterator[(String, Map[Symbol, LogNum])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEqualsSmart(Vector("dog" -> Map('N -> LogNum(3 / 3.)), "the" -> Map('D -> LogNum(5 / 5.)), "walks" -> Map('V -> LogNum(3 / 4.), 'N -> LogNum(1 / 4.))), iteratorSorted)

    assertEqualsSmart(Map('D -> LogNum(5 / 5.)), td.weights("the"))
    assertEqualsSmart(Map('N -> LogNum(3 / 3.)), td.weights("dog"))
    assertEqualsSmart(Map('V -> LogNum(3 / 4.), 'N -> LogNum(1 / 4.)), td.weights("walks"))
    assertEqualsSmart(Map('D -> LogNum(5 / 12.), 'N -> LogNum(4 / 12.), 'V -> LogNum(3 / 12.)), td.weights("aardvark"))
  }

  @Test
  def test_SimpleWeightedTagDictFactory_addOneSmoothingTransformer() {
    val td: WeightedTagDict[String, Symbol] =
      new SimpleWeightedTagDictFactory[String, Symbol](
        AddLambdaSmoothingCondCountsTransformer(1))
        .make(Seq(Vector(
          ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D), ("the", 'D),
          ("dog", 'N), ("dog", 'N), ("dog", 'N),
          ("walks", 'V), ("walks", 'V), ("walks", 'V), ("walks", 'N))))

    // TagDict methods

    assertEquals(Set('D, 'N, 'V), td.defaultSet)

    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("the"))
    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("dog"))
    assertEquals(Some(Set('D, 'N, 'V)), td.doGetSet("walks"))
    assertEquals(None, td.doGetSet("aardvark"))

    val setIterator: Iterator[(String, Set[Symbol])] = td.setIterator
    val setIteratorSorted = setIterator.toVector.sortBy(_._1)
    assertEquals(Vector("dog" -> Set('D, 'N, 'V), "the" -> Set('D, 'N, 'V), "walks" -> Set('D, 'N, 'V)), setIteratorSorted)

    assertEquals(Set('D, 'N, 'V), td.set("the"))
    assertEquals(Set('D, 'N, 'V), td.set("dog"))
    assertEquals(Set('D, 'N, 'V), td.set("walks"))
    assertEquals(Set('D, 'N, 'V), td.set("aardvark"))

    assertEquals(true, td.contains("the"))
    assertEquals(true, td.contains("dog"))
    assertEquals(true, td.contains("walks"))
    assertEquals(false, td.contains("aardvark"))

    assertEquals(true, td.contains("the", 'D))
    assertEquals(true, td.contains("the", 'N))
    assertEquals(true, td.contains("the", 'V))
    assertEquals(false, td.contains("the", 'Z))

    assertEquals(true, td.contains("dog", 'D))
    assertEquals(true, td.contains("dog", 'N))
    assertEquals(true, td.contains("dog", 'V))
    assertEquals(false, td.contains("dog", 'Z))

    assertEquals(true, td.contains("walks", 'D))
    assertEquals(true, td.contains("walks", 'N))
    assertEquals(true, td.contains("walks", 'V))
    assertEquals(false, td.contains("walks", 'Z))

    assertEquals(false, td.contains("aardvark", 'D))
    assertEquals(false, td.contains("aardvark", 'N))
    assertEquals(false, td.contains("aardvark", 'V))
    assertEquals(false, td.contains("aardvark", 'Z))

    assertEquals(Set("the", "dog", "walks"), td.symbols)

    assertEquals(Set('D, 'N, 'V), td.allTags)

    // WeightedTagDict methods

    assertEqualsSmart(Map('D -> LogNum(8 / 24.), 'N -> LogNum(7 / 24.), 'V -> LogNum(6 / 24.)), td.default)

    assertEqualsSmart(Some(Map('D -> LogNum(6 / 9.), 'N -> LogNum(1 / 9.), 'V -> LogNum(1 / 9.))), td.doGetMap("the"))
    assertEqualsSmart(Some(Map('D -> LogNum(1 / 7.), 'N -> LogNum(4 / 7.), 'V -> LogNum(1 / 7.))), td.doGetMap("dog"))
    assertEqualsSmart(Some(Map('D -> LogNum(1 / 8.), 'N -> LogNum(2 / 8.), 'V -> LogNum(4 / 8.))), td.doGetMap("walks"))
    assertEqualsSmart(None, td.doGetMap("aardvark"))

    val iterator: Iterator[(String, Map[Symbol, LogNum])] = td.iterator
    val iteratorSorted = iterator.toVector.sortBy(_._1)
    assertEqualsSmart(Vector(
      "dog" -> Map('D -> LogNum(1 / 7.), 'N -> LogNum(4 / 7.), 'V -> LogNum(1 / 7.)),
      "the" -> Map('D -> LogNum(6 / 9.), 'N -> LogNum(1 / 9.), 'V -> LogNum(1 / 9.)),
      "walks" -> Map('D -> LogNum(1 / 8.), 'N -> LogNum(2 / 8.), 'V -> LogNum(4 / 8.))), iteratorSorted)

    assertEqualsSmart(Map('D -> LogNum(6 / 9.), 'N -> LogNum(1 / 9.), 'V -> LogNum(1 / 9.)), td.weights("the"))
    assertEqualsSmart(Map('D -> LogNum(1 / 7.), 'N -> LogNum(4 / 7.), 'V -> LogNum(1 / 7.)), td.weights("dog"))
    assertEqualsSmart(Map('D -> LogNum(1 / 8.), 'N -> LogNum(2 / 8.), 'V -> LogNum(4 / 8.)), td.weights("walks"))
    assertEqualsSmart(Map('D -> LogNum(8 / 24.), 'N -> LogNum(7 / 24.), 'V -> LogNum(6 / 24.)), td.weights("aardvark"))
  }

  def assertEqualsSmart[A](expected: Map[A, LogNum], actual: Map[A, LogNum]) {
    assertEquals(expected.keys.toSet, actual.keys.toSet)
    for (k <- expected.keys) assertEqualsProb(expected(k), actual(k))
  }

  def assertEqualsSmart[A](expected: Option[Map[A, LogNum]], actual: Option[Map[A, LogNum]]) {
    assertEquals(expected.isDefined, actual.isDefined)
    if (expected.isDefined) assertEqualsSmart(expected.get, actual.get)
  }

  def assertEqualsSmart[A, B](expected: Vector[(A, Map[B, LogNum])], actual: Vector[(A, Map[B, LogNum])]) {
    assertEquals(expected.size, actual.size)
    for (((eA, eB), (aA, aB)) <- expected zip actual) {
      assertEquals(eA, aA)
      assertEqualsSmart(eB, aB)
    }
  }
}
