package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._

class FreqCountsTests {

  @Test
  def test_FreqCounts() {
    val f = Map('A' -> 2)
    val g = Map('B' -> 4)
    val h = Map('A' -> 3)
    val i = f +++ g +++ h
    assertEquals(Map('B' -> 4, 'A' -> 5), i.toMap)
  }

  @Test
  def test_FreqCounts_double() {
    val f = Map('A' -> 2.2)
    val g = Map('B' -> 4.4)
    val h = Map('A' -> 3.0)
    val i = f +++ g +++ h
    assertEquals(Map('B' -> 4.4, 'A' -> 5.2), i.toMap)
  }

  @Test
  def test_CondFreqCounts() {
    val f = CondFreqCounts(Map('A' -> Map('a -> 2)))
    val g = CondFreqCounts(Map('B' -> Map('a -> 3, 'b -> 4)))
    val h = CondFreqCounts(Map('A' -> Map('a -> 5), 'B' -> Map('b -> 6)))
    val i = f ++ g ++ h
    assertEquals(Map('A' -> Map('a -> 7), 'B' -> Map('a -> 3, 'b -> 10)), i.toMap)
  }

  @Test
  def test_CondFreqCounts_double() {
    val f = CondFreqCounts(Map('A' -> Map('a -> 2.1)))
    val g = CondFreqCounts(Map('B' -> Map('a -> 3.0, 'b -> 4.2)))
    val h = CondFreqCounts(Map('A' -> Map('a -> 5.5), 'B' -> Map('b -> 6.1)))
    val i = f ++ g ++ h
    assertEquals(Map('A' -> Map('a -> 7.6), 'B' -> Map('a -> 3.0, 'b -> 10.3)), i.toMap)
  }

}
