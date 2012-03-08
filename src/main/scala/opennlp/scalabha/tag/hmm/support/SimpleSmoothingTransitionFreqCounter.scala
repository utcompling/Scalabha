package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * CondFreqCounter decorator that smoothes transition counts.
 *
 * @param lambda			smoothing parameter
 * @param countsForBackoff	counts to be used to compute backoff information
 */
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, backoffCounts: FreqCounts[Tag, Int], numSingleCountItems: Map[Tag, Int], delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, backoffCounts, numSingleCountItems, delegate) {

  override protected def amendBackoffCounts(backoffCounts: FreqCounts[Tag, Int]) = backoffCounts - startEndTag

}
