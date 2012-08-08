package opennlp.scalabha.tag.support

import org.apache.commons.logging.LogFactory

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._

/**
 * Utilities for frequency distributions: functions to probabilities: P(B).
 */
object FreqDist {

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B) = 0 for all B.
   */
  def empty[B]: MultinomialFreqDist[B] = static(LogNum.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B) = v for all B.
   */
  def static[B](v: LogNum): MultinomialFreqDist[B] = new MultinomialFreqDist[B](Map(), v)

  /**
   * Construct a frequency distribution from the counts.  Calculates
   * the distribution by dividing each count by the total count.
   * P(B) = C(B) / Sum[C(x) for all x].
   *
   * Note that if the total count is zero, the distribution
   * returned is simply the empty distribution.
   *
   * @tparam B	the item being counted
   */
  def apply[B, N: Numeric](counts: Map[B, N]): MultinomialFreqDist[B] = {
    apply(DefaultedFreqCounts(counts))
  }

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
  def apply[B, N](resultCounts: DefaultedFreqCounts[B, N])(implicit num: Numeric[N]): MultinomialFreqDist[B] = {
    val DefaultedFreqCounts(counts, totalAddition, defaultCount) = resultCounts
    val total = resultCounts.total
    if (total == num.zero)
      FreqDist.empty[B]
    else {
      val logTotal = total.toLogNum
      new MultinomialFreqDist(counts.mapVals(_ / logTotal), defaultCount / logTotal)
    }
  }
}

class CondFreqDist[A, B](val dists: Map[A, MultinomialFreqDist[B]], val default: MultinomialFreqDist[B]) extends (A => MultinomialFreqDist[B]) {
  def apply(a: A) = dists.getOrElse(a, default)
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
  def empty[A, B]: A => MultinomialFreqDist[B] = static(LogNum.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B|A) = v for all A,B.
   */
  def static[A, B](v: LogNum): A => MultinomialFreqDist[B] = (_: Any) => FreqDist.static(v)

  /**
   * Construct a frequency distribution from the counts. Calculates
   * the distribution for each entry by dividing each count by the total
   * count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   *
   * Note that if the total for a given 'A' is zero, then
   * that 'A' will map to the empty distribution.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B, N: Numeric](counts: Map[A, Map[B, N]]): CondFreqDist[A, B] = {
    apply(DefaultedCondFreqCounts.fromMap(counts))
  }

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
   * that 'A' will map to the empty distribution.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B, N](resultCounts: DefaultedCondFreqCounts[A, B, N])(implicit num: Numeric[N]): CondFreqDist[A, B] = {
    val DefaultedCondFreqCounts(counts) = resultCounts
    val summedBackoffCounts = counts.values.foldLeft(DefaultedFreqCounts(Map[B, N](), num.zero, num.zero)) {
      case (DefaultedFreqCounts(zc, zt, zd), DefaultedFreqCounts(c, t, d)) =>
        DefaultedFreqCounts(zc +++ c, num.plus(zt, t), num.plus(zd, d))
    }
    new CondFreqDist(counts.mapVals(FreqDist(_)), FreqDist(summedBackoffCounts))
  }

  def unapply[A, B](cfd: CondFreqDist[A, B]) = Some(cfd.dists, cfd.default)
}
