package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.tag.support.CondFreqCounter
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.tag.support.SimpleSmoothingCondFreqCounter

/**
 * CondFreqCounter decorator that smoothes transition counts.
 *
 * @param lambda	smoothing parameter
 */
class SimpleSmoothingTransitionFreqCounter[Tag](lambda: Double, startEndTag: Tag, delegate: CondFreqCounter[Tag, Tag])
  extends SimpleSmoothingCondFreqCounter[Tag, Tag](lambda, delegate) {

  override protected def amendBackoffCounts(backoffCounts: Map[Tag, Double]) = backoffCounts - startEndTag

}
