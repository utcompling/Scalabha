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
          tag -> (tag match {
            case None => DefaultedFreqCounts(Map((None: Option[Sym]) -> 1.), 0., 0.)
            case _ => DefaultedFreqCounts(c + (None -> 0.), t, d)
          })
      })
  }

}

object EmissionCountsTransformer {
  def apply[Tag, Sym]() = {
    new EmissionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}

object TagDictConstrainedEmissionCountsTransformer {
  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag]): EmissionCountsTransformer[Tag, Sym] = {
    TagDictConstrainedEmissionCountsTransformer(tagDict,
      PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }

  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Sym]]): EmissionCountsTransformer[Tag, Sym] = {
    val c = (OptionalTagDict(tagDict).setIterator.ungroup.map(_.swap) :+ (None,None)).toSet.groupByKey
    new EmissionCountsTransformer(
      new ConstrainingCondCountsTransformer(c,
        delegate))
  }
}
