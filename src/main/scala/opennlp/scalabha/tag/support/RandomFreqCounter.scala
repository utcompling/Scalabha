package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import collection.mutable.{ Map => MMap }
import util.Random

/**
 * This counter multiplies each count received from its delegate by a
 * (possibly different) random number between 1 and maxCount.
 *
 */
class RandomCondFreqCounter[A, B](maxCount: Int, delegate: CondFreqCounter[A, B]) extends DelegatingCondFreqCounter[A, B](delegate) {
  private val rand = new Random(0) // static seed ensures results are reproducible

  override def resultCounts() = {
    val DefaultedCondFreqCounts(delegateResultCounts, delegateTotalAddition, delegateDefaultCount) = delegate.resultCounts
    val modifiedResultCounts =
      for ((a, DefaultedFreqCounts(aCounts, aTotalAdd, aDefault)) <- delegateResultCounts) yield {
        val scaled = FreqCounts(aCounts.toMap.mapValuesStrict(_ * (rand.nextInt(maxCount) + 1)))
        (a, DefaultedFreqCounts(scaled, aTotalAdd, aDefault))
      }
    DefaultedCondFreqCounts(modifiedResultCounts, delegateTotalAddition, delegateDefaultCount)
  }

}
