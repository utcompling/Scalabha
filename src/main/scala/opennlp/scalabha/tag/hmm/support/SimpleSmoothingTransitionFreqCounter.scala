package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondFreqCounter decorator that smoothes transition counts.
 *
 * @param lambda				smoothing parameter
 * @param numSingleCountItems	number of single-count Bs corresponding to each A.
 *                              More single-count items means more smoothing
 */
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, numSingleCountItems: Tag => Int, delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, numSingleCountItems, delegate) {

  override protected def amendBackoffCounts(backoffCounts: FreqCounts[Tag, Double]) = backoffCounts - startEndTag

}
