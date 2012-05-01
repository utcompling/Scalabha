package opennlp.scalabha.tag.hmm

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.DefaultedCondFreqCounts
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._

/**
 *
 */
class EmissionCountsTransformer[Tag, Sym](delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
  extends CondCountsTransformer[Option[Tag], Option[Sym]] {

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Sym], Double]) = {
    new DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, DefaultedFreqCounts(c, t, d)) =>
          //val results =
          tag -> (tag match {
            case None => DefaultedFreqCounts(Map((None: Option[Sym]) -> 1.), 0., 0.)
            case _ => DefaultedFreqCounts(c + (None -> 0.), t, d)
          })
        //println(results)
        //results
      })
  }

}

object EmissionCountsTransformer {
  def apply[Tag, Sym]() = {
    new EmissionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}

class TagDictConstrainedEmissionCountsTransformer[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
  extends CondCountsTransformer[Option[Tag], Option[Sym]] {

  private val optionalTagDict = OptionalTagDict(tagDict)

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Sym], Double]) = {
    new DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, DefaultedFreqCounts(c, t, d)) =>
          val filtered = c.filterKeys(sym => optionalTagDict(sym)(tag))
          tag -> (tag match {
            case None => DefaultedFreqCounts(filtered, 0., 0.)
            case _ => DefaultedFreqCounts(filtered, t, d)
          })
      })
  }

}

object TagDictConstrainedEmissionCountsTransformer {
  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag]) = {
    new TagDictConstrainedEmissionCountsTransformer(tagDict, PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}
