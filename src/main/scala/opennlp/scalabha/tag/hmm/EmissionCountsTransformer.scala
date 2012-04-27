package opennlp.scalabha.tag.hmm

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.DefaultedCondFreqCounts
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._

class EmissionCountsTransformer[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
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

object EmissionCountsTransformer {
  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag]) = {
    new EmissionCountsTransformer(tagDict, PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}
