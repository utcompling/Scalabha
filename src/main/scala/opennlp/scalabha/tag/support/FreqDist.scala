package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import org.apache.commons.logging.LogFactory

/**
 * Utilities for frequency distributions: functions to probabilities: P(B).
 */
object FreqDist {

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B) = 0 for all B.
   */
  val empty = static(LogNum.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B) = v for all B.
   */
  def static(v: LogNum) = (_: Any) => v

  /**
   * Construct a frequency distribution from the counter result.  Calculates
   * the distribution by dividing each count by the total count.
   * P(B) = C(B) / Sum[C(x) for all x].
   * Arguments should come from a call to FreqCounter.resultCounts.
   *
   * The "totalAddition" portion is added to the total count before
   * dividing.  The "defaultCount" is used as the count for "unseen" items,
   * those items not included in the counts.
   *
   * Note that if the total (after additions) is zero, the distribution
   * returned is simply the empty distribution.
   *
   * @tparam B	the item being counted
   */
  def apply[B](resultCounts: DefaultedFreqCounts[B, Double]): B => LogNum = {
    val DefaultedFreqCounts(counts, totalAddition, defaultCount) = resultCounts
    val total = counts.toMap.values.sum + totalAddition
    if (total == 0)
      FreqDist.empty
    else
      counts.toMap.mapValuesStrict(count => (count / total).toLogNum)
        .withDefaultValue((defaultCount / total).toLogNum)
  }
}

/**
 * Utilities for conditional frequency distributions: functions to functions
 * to probabilities: P(B|A).
 */
object CondFreqDist {
  private val LOG = LogFactory.getLog(CondFreqDist.getClass)

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B|A) = 0 for all A,B.
   */
  val empty = static(LogNum.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B|A) = v for all A,B.
   */
  def static(v: LogNum) = (_: Any) => (_: Any) => v

  /**
   * Construct a frequency distribution from the counter result. Calculates
   * the distribution for each entry by dividing each count by the total
   * count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   * Argument should be from a call to CondFreqDist.resultCounts.
   *
   * The "totalAddition" portions at each level are added to the total
   * counts before dividing.  The grand total includes the additions to each
   * individual 'A' entry.  The "defaultCount" at each level are used as
   * the count for "unseen" items, those items not included in the counts.
   *
   * Note that if the total for a given 'A' (after additions) is zero, then
   * that 'A' will map to the empty distribution.  If the grand total
   * (including additions from the top level and all 'A' entries) is zero,
   * then the empty conditional distribution is returned.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B](resultCounts: DefaultedCondFreqCounts[A, B, Double]): A => B => LogNum = {
    resultCounts.counts.mapValuesStrict(FreqDist(_)).withDefaultValue(FreqDist.empty)
  }
}
