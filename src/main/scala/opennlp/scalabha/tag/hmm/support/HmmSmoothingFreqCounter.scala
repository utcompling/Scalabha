package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondFreqCounter decorator that fixes start/end counts for emissions counts.
 */
class StartEndFixingEmissionFreqCounter[Tag, Sym](startEndSymbol: Sym, startEndTag: Tag, delegate: CondFreqCounter[Tag, Sym])
  extends DelegatingCondFreqCounter[Tag, Sym](delegate) {

  override def resultCounts() = {
    val DefaultedCondFreqCounts(superResultCounts, totalAddition, defaultCount) = delegate.resultCounts()

    val corrected =
      superResultCounts.mapValuesStrict {
        case DefaultedFreqCounts(aCounts, aTotalAdd, aDefault) =>
          // Remove startEndTag from association with any symbol
          DefaultedFreqCounts(aCounts - startEndSymbol, aTotalAdd, aDefault)
      }

    // Make sure startEndSymbol maps only to startEndTag, and with Probability=1
    DefaultedCondFreqCounts(corrected + (startEndTag -> DefaultedFreqCounts(Map(startEndSymbol -> 2.0), 0, 0)), totalAddition, defaultCount)
  }

}
