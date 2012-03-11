package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import org.apache.commons.logging.LogFactory

/**
 * Utilities for frequency distributions: functions to probabilities: P(B).
 */
object FreqDist {

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B) = 0 for all B.
   */
  val empty = static(Probability.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B) = v for all B.
   */
  def static(v: Probability) = (_: Any) => v

  /**
   * Construct a frequency distribution from the counter result.  Calls
   * FreqCounter.resultCounts and calculates the distribution by dividing
   * each count by the total count.  P(B) = C(B) / Sum[C(x) for all x].
   *
   * The "totalAddition" portion is added to the total count before
   * dividing.  The "defaultCount" is used as the count for "unseen" items,
   * those items not included in the counts.
   *
   * Note that if the total (after additions) is zero, the distribution
   * returned is simply the empty distribution.
   */
  def apply[B](counter: FreqCounter[B]): B => Probability = {
    apply(counter.resultCounts)
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
   */
  def apply[B](resultCounts: DefaultedFreqCounts[B, Double]): B => Probability = {
    val DefaultedFreqCounts(counts, totalAddition, defaultCount) = resultCounts
    val total = counts.toMap.values.sum + totalAddition
    if (total == 0)
      FreqDist.empty
    else
      counts.toMap.mapValuesStrict(count => (count / total).toProbability)
        .withDefaultValue((defaultCount / total).toProbability)
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
  val empty = static(Probability.zero)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B|A) = v for all A,B.
   */
  def static(v: Probability) = (_: Any) => (_: Any) => v

  /**
   * Construct a frequency distribution from the counter result.  Calls
   * CondFreqCounter.resultCounts and calculates the distribution for each
   * entry by dividing each count by the total count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   *
   * The "totalAddition" portions at each level are added to the total
   * counts before dividing.  The "defaultCount" at each level are used as
   * the count for "unseen" items, those items not included in the counts.
   *
   * Note that if the total for a given 'A' (after additions) is zero, then
   * that 'A' will map to the empty distribution.  If the grand total
   * (including additions from the top level and all 'A' entries) is zero,
   * then the empty conditional distribution is returned.
   */
  def apply[A, B](counter: CondFreqCounter[A, B]): A => B => Probability = {
    apply(counter.resultCounts)
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
   * that 'A' will map to the empty distribution.  If the grand total
   * (including additions from the top level and all 'A' entries) is zero,
   * then the empty conditional distribution is returned.
   */
  def apply[A, B](resultCounts: DefaultedCondFreqCounts[A, B, Double]): A => B => Probability = {
    val DefaultedCondFreqCounts(counts, totalAddition, defaultCount) = resultCounts
    val (dists: List[(A, B => Probability)], total) =
      counts.foldLeft(List[(A, B => Probability)](), totalAddition) {
        case ((dists, total), (a, DefaultedFreqCounts(aCounts, aTotalAddition, aDefaultCount))) =>
          val aTotal = aCounts.toMap.values.sum + aTotalAddition
          val aDistDefaulted =
            if (aTotal == 0)
              FreqDist.empty
            else
              aCounts.toMap.mapValuesStrict(count => (count / aTotal).toProbability)
                .withDefaultValue((aDefaultCount / aTotal).toProbability)

          if (LOG.isDebugEnabled && Set("NN", "DT", "N", "D").contains(a.asInstanceOf[String])) {
            LOG.debug("    tag = " + a)
            LOG.debug("        aCounts = " + aCounts.toMap.asInstanceOf[Map[String, Double]].toList.sorted.takeRight(10).map { case (k, v) => "%s -> %.2f".format(k, v) })
            LOG.debug("        aDefaultCount = " + aDefaultCount)
            LOG.debug("        aTotal = " + aTotal)
            aDistDefaulted match {
              case x: Map[String, Probability] =>
                LOG.debug("        aDistDefaulted = " + x.toList.sorted.takeRight(10).map { case (b, p) => "%s -> %.2f".format(b, p.underlying) })
              case _ =>
                LOG.debug("        empty FreqDist")
            }

            LOG.debug("")

            if (!aCounts.toMap.contains("N".asInstanceOf[B]) && !aCounts.toMap.contains("NN".asInstanceOf[B])) {
              for (w <- List("the", "company", "zzzzzzz").map(_.asInstanceOf[B])) {
                LOG.debug("        p(%s|%s) = c(%s,%s) / c(%s) = %.2f / %.2f = %.2f (%.2f)"
                  .format(
                    w, a, a, w, a,
                    aCounts.toMap.getOrElse(w, aDefaultCount), aTotal,
                    aDistDefaulted(w).toDouble, aDistDefaulted(w).underlying))
              }
              LOG.debug("")
            }
          }

          ((a, aDistDefaulted) :: dists, total + aTotal)
      }
    if (total == 0)
      CondFreqDist.empty
    else
      dists.toMap.withDefaultValue(FreqDist.static((defaultCount / total).toProbability))
  }
}
