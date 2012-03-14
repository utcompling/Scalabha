package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondCountsTransformer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondCountsTransformer decorator that fixes start/end counts for emissions counts.
 */
case class StartEndFixingEmissionCountsTransformer[Tag, Sym](startEndSymbol: Sym, startEndTag: Tag, delegate: CondCountsTransformer[Tag, Sym])
  extends CondCountsTransformer[Tag, Sym] {

  override def apply(counts: DefaultedCondFreqCounts[Tag, Sym, Double]) = {
    val corrected =
      delegate(counts).counts.mapValuesStrict {
        case DefaultedFreqCounts(aCounts, aTotalAdd, aDefault) =>
          // Remove startEndTag from association with any symbol
          DefaultedFreqCounts(aCounts - startEndSymbol, aTotalAdd, aDefault)
      }

    // Make sure startEndSymbol maps only to startEndTag, and with Probability=1
    new DefaultedCondFreqCounts(corrected + (startEndTag -> DefaultedFreqCounts(Map(startEndSymbol -> 2.), 0., 0.)))
  }

}

object StartEndFixingEmissionCountsTransformer {
  def apply[Tag, Sym](startEndSymbol: Sym, startEndTag: Tag): StartEndFixingEmissionCountsTransformer[Tag, Sym] = {
    StartEndFixingEmissionCountsTransformer(startEndSymbol, startEndTag, PassthroughCondCountsTransformer[Tag, Sym]())
  }
}
