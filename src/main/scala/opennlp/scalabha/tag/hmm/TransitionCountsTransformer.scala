package opennlp.scalabha.tag.hmm

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.DefaultedCondFreqCounts
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._

class TransitionCountsTransformer[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Tag]])
  extends CondCountsTransformer[Option[Tag], Option[Tag]] {

  val constraints = {
    val allTags = OptionalTagDict(tagDict).allTags
    allTags.mapTo(_ => (allTags + None)).toMap + (None -> allTags)
  }

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Tag], Double]) = {
    new DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, DefaultedFreqCounts(c, t, d)) =>
          val filtered = c.filterKeys(constraints(tag))
          //val results =
          tag -> (tag match {
            case None => DefaultedFreqCounts(filtered + (None -> 0.), t, d)
            case _ => DefaultedFreqCounts(filtered, t, d)
          })
        //println(results)
        //results
      })
  }

}

object TransitionCountsTransformer {
  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag]) = {
    new TransitionCountsTransformer(tagDict, PassthroughCondCountsTransformer[Option[Tag], Option[Tag]]())
  }
}
