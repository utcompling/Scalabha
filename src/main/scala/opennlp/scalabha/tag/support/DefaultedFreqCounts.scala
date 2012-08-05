package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * @tparam B	the item being counted
 * @tparam N	the Numeric type of the count
 */
case class DefaultedFreqCounts[B, N](counts: Map[B, N], totalAddition: N, defaultCount: N)(implicit num: Numeric[N]) {
  def +++(that: DefaultedFreqCounts[B, N]) = {
    val countSum = counts +++ that.counts

    if (totalAddition == num.zero && defaultCount == num.zero)
      new DefaultedFreqCounts(countSum, that.totalAddition, that.defaultCount)

    else if (that.totalAddition == num.zero && that.defaultCount == num.zero)
      new DefaultedFreqCounts(countSum, totalAddition, defaultCount)

    else {
      assert(totalAddition == that.totalAddition)
      assert(defaultCount == that.defaultCount)
      new DefaultedFreqCounts(countSum, totalAddition, defaultCount)
    }
  }

  def simpleCounts = counts

  def total = num.plus(counts.values.sum, totalAddition)
}

object DefaultedFreqCounts {
  def apply[B, N: Numeric](counts: Map[B, N]) =
    new DefaultedFreqCounts(counts, implicitly[Numeric[N]].zero, implicitly[Numeric[N]].zero)
}

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 * @tparam N	the Numeric type of the count
 */
case class DefaultedCondFreqCounts[A, B, N: Numeric](counts: Map[A, DefaultedFreqCounts[B, N]], unseenContextProb: LogNum) {
  def +++(that: DefaultedCondFreqCounts[A, B, N]): DefaultedCondFreqCounts[A, B, N] = {
    val countsSum = (counts.iterator ++ that.counts).groupByKey.mapVals(_.reduce(_ +++ _))

    if (unseenContextProb == implicitly[Numeric[N]].zero)
      DefaultedCondFreqCounts(countsSum, unseenContextProb)

    else if (that.unseenContextProb == implicitly[Numeric[N]].zero)
      DefaultedCondFreqCounts(countsSum, that.unseenContextProb)

    else {
      assert(unseenContextProb == that.unseenContextProb)
      DefaultedCondFreqCounts(countsSum, unseenContextProb)
    }
  }

  def simpleCounts = counts.mapVals(_.simpleCounts)
}

object DefaultedCondFreqCounts {
  def apply[A, B, N: Numeric](counts: Map[A, DefaultedFreqCounts[B, N]]): DefaultedCondFreqCounts[A, B, N] =
    DefaultedCondFreqCounts(counts, LogNum.zero)
  def fromMap[A, B, N: Numeric](counts: Map[A, Map[B, N]]): DefaultedCondFreqCounts[A, B, N] =
    DefaultedCondFreqCounts(counts.mapVals(c => DefaultedFreqCounts(c)), LogNum.zero)
  def fromMap[A, B, N: Numeric](counts: Map[A, Map[B, N]], unseenContextProb: LogNum): DefaultedCondFreqCounts[A, B, N] =
    DefaultedCondFreqCounts(counts.mapVals(c => DefaultedFreqCounts(c)), unseenContextProb)
}
