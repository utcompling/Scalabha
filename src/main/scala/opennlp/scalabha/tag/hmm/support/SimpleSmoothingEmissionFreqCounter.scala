package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondFreqCounter decorator that smoothes emission counts.
 *
 * @param lambda			smoothing parameter
 * @param countsForBackoff	counts to be used to compute backoff information
 */
class SimpleSmoothingEmissionFreqCounter[Tag, Sym](lambda: Double, startEndSymbol: Sym, startEndTag: Tag, backoffCounts: FreqCounts[Sym, Int], numSingleCountItems: Map[Tag, Int], delegate: CondFreqCounter[Tag, Sym])
  extends SimpleSmoothingCondFreqCounter[Tag, Sym](lambda, backoffCounts, numSingleCountItems, delegate) {

  override protected def getDelegateResultCounts() =
    removeStartEnds(super.getDelegateResultCounts)

  override def resultCounts() =
    removeStartEnds(super.resultCounts)

  private def removeStartEnds(resultCounts: DefaultedCondFreqCounts[Tag, Sym, Double]): DefaultedCondFreqCounts[Tag, Sym, Double] = {
    val DefaultedCondFreqCounts(superResultCounts, totalAddition, defaultCount) = resultCounts

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
