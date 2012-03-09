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
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, tagDict: Map[Tag, Set[Tag]], backoffData: Iterable[Iterable[Tag]], delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, tagDict, backoffData, delegate) {

  override protected def amendBackoffCounts(backoffCounts: FreqCounts[Tag, Double]) = backoffCounts - startEndTag

}
