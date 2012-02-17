package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.tag.support.SimpleSmoothingCondFreqCounter
import opennlp.scalabha.tag.support.FreqCounts

/**
 * CondFreqCounter decorator that smoothes transition counts.
 *
 * @param lambda	smoothing parameter
 */
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, delegate) {

  override protected def amendBackoffCounts(backoffCounts: FreqCounts[Tag, Double]) = backoffCounts - startEndTag

}
