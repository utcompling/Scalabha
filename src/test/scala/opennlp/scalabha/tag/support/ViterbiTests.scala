package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit.Test
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.tag._

class ViterbiTests {

  @Test
  def test_apply() {

    type Sym = String
    type Tag = Symbol

    val fullTagset = Set('D, 'A, 'N, 'V)
    val fullTagDict = OptionalTagDict(SimpleTagDict(Map(
      "the" -> Set('D),
      "big" -> Set('A),
      "dog" -> Set('N),
      "walks" -> Set('N, 'V))))
    val incompleteTagDict = OptionalTagDict(SimpleTagDict(Map(
      "the" -> Set('D),
      "dog" -> Set('A),
      "walks" -> Set('V))))

    val largeTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('A), Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('N), Some('V)),
      Some('V) -> Set(None))
    val smallTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('A), Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(None))
    val incompleteTagTransitions = Map[Option[Tag], Set[Option[Tag]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('N)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(None))

    val v = new Viterbi[Sym, Tag](new TagEdgeScorer[Sym, Tag] {
      override def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]) = {
        (prevTag, currTag) match {
          case (None, Some('D)) => LogNum(1.)
          case (Some('D), Some('A)) => LogNum(.25)
          case (Some('D), Some('N)) => LogNum(.75)
          case (Some('A), Some('N)) => LogNum(1.)
          case (Some('N), Some('N)) => LogNum(.45)
          case (Some('N), Some('V)) => LogNum(.55)
          case (Some('V), None) => LogNum(1.)
          case _ => LogNum.zero
        }
      }
    })

    assertEquals(Some(List('D, 'N, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagset))
    assertEquals(Some(List('D, 'A, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagDict))
    assertEquals(Some(List('D, 'D, 'A, 'V)), v.tagSequence("the big dog walks".split(" "), incompleteTagDict))
    assertEquals(Some(List('D, 'N, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagset, largeTagTransitions))
    assertEquals(Some(List('D, 'A, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagDict, largeTagTransitions))
    assertEquals(None, v.tagSequence("the big dog walks".split(" "), incompleteTagDict, largeTagTransitions))
    assertEquals(Some(List('D, 'A, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagset, smallTagTransitions))
    assertEquals(Some(List('D, 'A, 'N, 'V)), v.tagSequence("the big dog walks".split(" "), fullTagDict, smallTagTransitions))
    assertEquals(None, v.tagSequence("the big dog walks".split(" "), incompleteTagDict, smallTagTransitions))
    assertEquals(None, v.tagSequence("the big dog walks".split(" "), fullTagset, incompleteTagTransitions))
    assertEquals(None, v.tagSequence("the big dog walks".split(" "), fullTagDict, incompleteTagTransitions))
    assertEquals(None, v.tagSequence("the big dog walks".split(" "), incompleteTagDict, incompleteTagTransitions))

  }

}
