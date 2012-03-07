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
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, countsForBackoff: CondFreqCounts[Tag, Tag, Int], delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, countsForBackoff, delegate) {

  override protected def amendBackoffCounts(backoffCounts: FreqCounts[Tag, Int]) = backoffCounts - startEndTag

}
