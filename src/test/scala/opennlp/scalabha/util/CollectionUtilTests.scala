package opennlp.scalabha.util

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.prop.Checkers
import org.junit.Test
import org.junit.Assert._
import opennlp.scalabha.util.CollectionUtil._
import scala.collection.mutable.SetBuilder

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
  def test_dropRightWhile() {
    val coll1 = List(1, 2, 3, 4, 5)
    val res1: List[Int] = coll1.dropRightWhile(_ == 0)
    assertEquals(List(1, 2, 3, 4, 5), res1)

    val coll2 = Vector(1, 2, 3, 4, 5)
    val res2: Vector[Int] = coll2.dropRightWhile(_ > 3)
    assertEquals(Vector(1, 2, 3), res2)

    val coll3 = " this  and that "
    val res3: String = coll3.dropRightWhile(_ == ' ')
    assertEquals(" this  and that", res3)
  }

  @Test
  def test_splitAt() {
    val (first1: Iterator[Int], second1: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertEqualsIterator(Iterator(1, 2, 3), first1)
    assertEqualsIterator(Iterator(4, 5), second1)

    val (first2: Iterator[Int], second2: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(3)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has NOT YET been read completely", e.getMessage)
    }
    assertEquals(1, first2.next)
    assertEquals(2, first2.next)
    assertEquals(3, first2.next)
    assertException(first2.next) {
      case e: AssertionError => assertEquals("assertion failed: first has already been read completely", e.getMessage)
    }
    assertEquals(4, second2.next)
    assertEquals(5, second2.next)
    assertException(second2.next) {
      case e: AssertionError => assertEquals("assertion failed: second has already been read completely", e.getMessage)
    }

    val (first3: Iterator[Int], second3: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(-1)
    assertEqualsIterator(Iterator(), first3)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second3)

    val (first4: Iterator[Int], second4: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(0)
    assertEqualsIterator(Iterator(), first4)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), second4)

    val (first5: Iterator[Int], second5: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(5)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first5)
    assertEqualsIterator(Iterator(), second5)

    val (first6: Iterator[Int], second6: Iterator[Int]) = Iterator(1, 2, 3, 4, 5).splitAt(6)
    assertEqualsIterator(Iterator(1, 2, 3, 4, 5), first6)
    assertEqualsIterator(Iterator(), second6)
  }

  @Test
  def test_zipSafe() {
    val a = Vector(1, 2, 3)
    val b = List('a, 'b, 'c)

    val res1: Iterator[(Int, Symbol)] = a.iterator zipSafe b
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res1)

    val res2: Vector[(Int, Symbol)] = a zipSafe b
    assertEquals(Vector(1 -> 'a, 2 -> 'b, 3 -> 'c), res2)

    val res3: Iterator[(Int, Symbol)] = (a.iterator, b).zipSafe
    assertEqualsIterator(Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c), res3)

    val res4: List[(Int, Symbol)] = (a.toList, b).zipSafe
    assertEquals(List(1 -> 'a, 2 -> 'b, 3 -> 'c), res4)

    val c = Vector(1, 2, 3, 4)

    val res5: Iterator[(Symbol, Int)] = b.iterator zipSafe c
    assertEquals(('a, 1), res5.next)
    assertEquals(('b, 2), res5.next)
    assertEquals(('c, 3), res5.next)
    assertException(res5.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res6: Iterator[(Int, Symbol)] = c.iterator zipSafe b
    assertEquals((1, 'a), res6.next)
    assertEquals((2, 'b), res6.next)
    assertEquals((3, 'c), res6.next)
    assertException(res6.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException(b zipSafe c) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException(c zipSafe b) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res7: Iterator[(Symbol, Int)] = (b.iterator, c).zipSafe
    assertEquals(('a, 1), res7.next)
    assertEquals(('b, 2), res7.next)
    assertEquals(('c, 3), res7.next)
    assertException(res7.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    val res8: Iterator[(Int, Symbol)] = (c.iterator, b).zipSafe
    assertEquals((1, 'a), res8.next)
    assertEquals((2, 'b), res8.next)
    assertEquals((3, 'c), res8.next)
    assertException(res8.next) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException((b, c).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }

    assertException((c, b).zipSafe) {
      case e: AssertionError => assertEquals("assertion failed: Attempting to zipSafe collections of different lengths.", e.getMessage)
    }
  }

  @Test
  def test_unzip() {
    val itr1 = Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val (a1: Vector[Int], b1: Vector[Symbol]) = itr1.unzip
    assertEquals(Vector(1, 2, 3), a1)
    assertEquals(Vector('a, 'b, 'c), b1)

    import collection.mutable.{ ListBuffer, SetBuilder }
    val itr2 = Iterator(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val (a2: List[Int], b2: Set[Symbol]) = itr2.unzip(ListBuffer(), new SetBuilder(Set[Symbol]()))
    assertEquals(List(1, 2, 3), a2)
    assertEquals(Set('a, 'b, 'c), b2)
  }

  @Test
  def test_mapKeys() {
    val coll1 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res1: Map[Int, Symbol] = coll1.mapKeys(_ + 2)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res1)

    val coll2 = List(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res2: List[(Int, Symbol)] = coll2.mapKeys(_ + 2)
    assertEquals(List(3 -> 'a, 4 -> 'b, 5 -> 'c), res2)

    var callCount = 0
    val coll3 = Map(1 -> 'a, 2 -> 'b, 3 -> 'c)
    val res3: Map[Int, Symbol] = coll3.mapKeys(i => { callCount += 1; i + 2 })
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(Map(3 -> 'a, 4 -> 'b, 5 -> 'c), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_mapVals() {
    val coll1 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res1: Map[Symbol, Int] = coll1.mapVals(_ + 2)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res1)

    val coll2 = List('a -> 1, 'b -> 2, 'c -> 3)
    val res2: List[(Symbol, Int)] = coll2.mapVals(_ + 2)
    assertEquals(List('a -> 3, 'b -> 4, 'c -> 5), res2)

    var callCount = 0
    val coll3 = Map('a -> 1, 'b -> 2, 'c -> 3)
    val res3: Map[Symbol, Int] = coll3.mapVals(i => { callCount += 1; i + 2 })
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(Map('a -> 3, 'b -> 4, 'c -> 5), res3)
    assertEquals(3, callCount)
  }

  @Test
  def test_avg() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: Double = coll1.avg
    assertEquals(2.5, avg1, 0.0000001)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Float = coll2.avg
    assertEquals(2.5f, avg2, 0.0000001)
  }

  @Test
  def test_normalize() {
    val coll1 = List(1, 2, 2, 5)
    val avg1: List[Double] = coll1.normalize
    assertEquals(List(0.1, 0.2, 0.2, 0.5), avg1)

    val coll2 = Set(1.0f, 1.5f, 2.5f, 5.0f)
    val avg2: Set[Float] = coll2.normalize
    assertEquals(Set(0.1f, 0.15f, 0.25f, 0.5f), avg2)
  }

  @Test
  def test_normalizeValues() {
    val coll1 = List('a -> 1, 'b -> 2, 'c -> 2, 'd -> 5)
    val avg1: List[(Symbol, Double)] = coll1.normalizeValues
    assertEquals(List('a -> 0.1, 'b -> 0.2, 'c -> 0.2, 'd -> 0.5), avg1)

    val coll2 = Set('a -> 1.0f, 'b -> 1.5f, 'c -> 2.5f, 'd -> 5.0f)
    val avg2: Set[(Symbol, Float)] = coll2.normalizeValues
    assertEquals(Set('a -> 0.1f, 'b -> 0.15f, 'c -> 0.25f, 'd -> 0.5f), avg2)

    val coll3 = Map('a -> 1.0, 'b -> 1.5, 'c -> 2.5, 'd -> 5.0)
    val avg3: Map[Symbol, Double] = coll3.normalizeValues
    assertEquals(Map('a -> 0.1, 'b -> 0.15, 'c -> 0.25, 'd -> 0.5), avg3)
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
    assertEquals("Iterators have different lengths", expected.hasNext, actual.hasNext)
  }

}
