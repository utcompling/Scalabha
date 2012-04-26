package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondCountsTransformer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondCountsTransformer decorator that fixes start/end counts for emissions counts.
 */
case class StartEndFixingEmissionCountsTransformer[Tag, Sym](delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
  extends CondCountsTransformer[Option[Tag], Option[Sym]] {

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Sym], Double]) = {
    val corrected =
      delegate(counts).counts.mapValuesStrict {
        case DefaultedFreqCounts(aCounts, aTotalAdd, aDefault) =>
          // Remove startEndTag from association with any symbol
          DefaultedFreqCounts(aCounts - None, aTotalAdd, aDefault)
      }

    // Make sure startEndSymbol maps only to startEndTag, and with Probability=1
    new DefaultedCondFreqCounts(corrected + (None -> DefaultedFreqCounts(Map(None -> 2.), 0., 0.)))
  }

}

object StartEndFixingEmissionCountsTransformer {
  def apply[Tag, Sym](): StartEndFixingEmissionCountsTransformer[Tag, Sym] = {
    StartEndFixingEmissionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}
