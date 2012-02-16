package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.tag.support.SimpleSmoothingCondFreqCounter

/**
 * CondFreqCounter decorator that smoothes emission counts.
 *
 * @param lambda	smoothing parameter
 */
class SimpleSmoothingEmissionFreqCounter[Sym, Tag](lambda: Double, startEndSymbol: Sym, startEndTag: Tag, delegate: CondFreqCounter[Sym, Tag])
  extends SimpleSmoothingCondFreqCounter[Sym, Tag](lambda, delegate) {

  override protected def getDelegateResultCounts() =
    removeStartEnds(super.getDelegateResultCounts)

  override def resultCounts() =
    removeStartEnds(super.resultCounts)

  private def removeStartEnds(resultCounts: (Map[Sym, (Map[Tag, Double], Double, Double)], Double, Double)): (Map[Sym, (Map[Tag, Double], Double, Double)], Double, Double) = {
    val (superResultCounts, totalAddition, defaultCount) = resultCounts

    val corrected =
      superResultCounts.mapValuesStrict {
        case (aCounts, aTotalAdd, aDefault) =>
          // Remove startEndTag from association with any symbol
          (aCounts - startEndTag, aTotalAdd, aDefault)
      }

    // Make sure startEndSymbol maps only to startEndTag, and with Probability=1
    (corrected + (startEndSymbol -> (Map(startEndTag -> 2.0), 0, 0)), totalAddition, defaultCount)
  }

}
