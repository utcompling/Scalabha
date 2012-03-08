package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._

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
          println("tag = " + a)
          println("    aCounts = " + aCounts.toMap.size)
          val aTotal = aCounts.toMap.values.sum + aTotalAddition.toDouble
          val aDistDefaulted =
            if (aTotal == 0)
              FreqDist.empty
            else {
              val x =
                aCounts.toMap.mapValuesStrict(count => (count.toDouble / aTotal).toProbability)
                  .withDefaultValue((aDefaultCount.toDouble / aTotal).toProbability)
              println("    x = " + x.toMap.asInstanceOf[Map[String, String]].toList.take(10))
              x
            }
          ((a, aDistDefaulted) :: dists, total + aTotal)
      }
    if (total == 0)
      CondFreqDist.empty
    else
      dists.toMap.withDefaultValue(FreqDist.static((defaultCount / total).toProbability))
  }
}
