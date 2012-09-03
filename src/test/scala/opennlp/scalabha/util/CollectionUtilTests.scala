package opennlp.scalabha.util

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.prop.Checkers
import org.junit.Test
import org.junit.Assert._
import opennlp.scalabha.util.CollectionUtil._

class CollectionUtilTests {

  @Test
  def test_toTuple() {
    val seq = Seq(1, 2)
    val seqT: (Int, Int) = seq.toTuple2
    assertEquals((1, 2), seqT)

    assertException(seq.toTuple3) {
      case e: AssertionError => assertEquals("Cannot convert sequence of length 2 into Tuple3", e.getMessage)
    }

    val arr = Array("3", "4", "5", "6")
    val arrT: (String, String, String, String) = arr.toTuple4
    assertEquals(("3", "4", "5", "6"), arrT)

    assertException(arr.toTuple5) {
      case e: AssertionError => assertEquals("Cannot convert array of length 4 into Tuple5", e.getMessage)
    }
  }

  @Test
  def test_prependAppendIterator() {
    val new1: Iterator[Int] = 1 +: Iterator(3, 4, 5)
    assertEqualsIterator(Iterator(1, 3, 4, 5), new1)

    val new2: Iterator[Int] = Iterator(3, 4, 5) :+ 7
    assertEqualsIterator(Iterator(3, 4, 5, 7), new2)

    val new3: Iterator[Int] = 1 +: 2 +: Iterator(3, 4, 5) :+ 6 :+ 7
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5, 6, 7), new3)
  }

  @Test
  def test_groupByKey() {
    val coll1 = Vector(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 2 -> 'b, 3 -> 'd)
    val grouped1: Map[Int, Vector[Symbol]] = coll1.groupByKey
    assertEquals(Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd)), grouped1)

    val coll2 = Set(1 -> 'a, 2 -> 'b, 3 -> 'c, 2 -> 'a, 3 -> 'd)
    val grouped2: Map[Int, Set[Symbol]] = coll2.groupByKey
    assertEquals(Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd)), grouped2)
  }

  @Test
  def test_ungroup() {
    val grouped1 = Map(1 -> Vector('a), 2 -> Vector('b, 'a, 'b), 3 -> Vector('c, 'd))
    val coll1: Iterator[(Int, Symbol)] = grouped1.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 2 -> 'b, 3 -> 'c, 3 -> 'd), coll1)

    val grouped2 = Map(1 -> Set('a), 2 -> Set('b, 'a), 3 -> Set('c, 'd))
    val coll2: Iterator[(Int, Symbol)] = grouped2.ungroup
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 2 -> 'a, 3 -> 'c, 3 -> 'd), coll2)
  }

  @Test
  def test_toVector() {
    val coll1 = List(1, 2, 3)
    val vec1: Vector[Int] = coll1.toVector
    assertEquals(Vector(1, 2, 3), vec1)

    val coll2 = Array('a, 'b, 'c)
    val vec2: Vector[Symbol] = coll2.toVector
    assertEquals(Vector('a, 'b, 'c), vec2)
  }

  def assertException(block: => Unit)(handle: PartialFunction[Throwable, Unit]) {
    try { block; fail("no exception thrown") } catch (handle)
  }

  def assertEqualsIterator[A](expected: Iterator[A], actual: Iterator[A]) {
    while (expected.hasNext && actual.hasNext)
      assertEquals(expected.next, actual.next)
    assertEquals(expected.hasNext, actual.hasNext)
  }

}
