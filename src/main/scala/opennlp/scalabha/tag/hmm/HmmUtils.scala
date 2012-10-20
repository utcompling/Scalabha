package opennlp.scalabha.tag.hmm

import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.TagUtils._
import opennlp.scalabha.tag.TagDict.OptionalTagDict
import opennlp.scalabha.util.LogNum

object HmmUtils {

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  def getCountsFromTagged[Sym, Tag](taggedSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    // Separate symbols from tags.  Add start/final symbols and tags to each sequence
    val endedSequences = taggedSequences.map(((None -> None) +: _.map { case (s, t) => Some(s) -> Some(t) } :+ (None -> None)))
    getCountsFromEndedTagged(endedSequences)
  }

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  def getCountsFromEndedTagged[Sym, Tag](endedTaggedSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    // Get the tag transitions, including start/final tags
    val tagPairs = endedTaggedSequences.map(_.map(_._2).sliding2).flatten
    val transitionCounts = tagPairs.groupByKey.mapVals(_.counts)

    // Get the word/tag pairs (emissions)
    val tagSymbolPairs = endedTaggedSequences.flatMap(_.map(_.swap))
    val emissionCounts = tagSymbolPairs.groupByKey.mapVals(_.counts)

    (transitionCounts, emissionCounts)
  }

  /**
   * Make a uniform transition distribution mapping every tag to every other
   * tag (but without mapping None -> None).
   */
  def uniformTransitionDist[Tag](tagset: Set[Tag]) = {
    val allTags: Set[Option[Tag]] = tagset.map(Some(_))
    CondFreqDist(allTags.mapToVal((allTags + None).mapToVal(1.).toMap).toMap + (None -> allTags.mapToVal(1.).toMap))
  }

  def addDistributionsToRawSequences[Sym, Tag](
    rawSequences: Seq[IndexedSeq[Sym]],
    tagDict: OptionalTagDict[Sym, Tag],
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): Seq[IndexedSeq[(Option[Sym], Vector[(Option[Tag], (Map[Option[Tag], LogNum], LogNum))])]] = {
    val allTags = tagDict.allTags + None
    addDistributionsToRawSequences(rawSequences, tagDict, transitions, emissions, allTags.mapToVal(allTags).toMap)
  }

  def addDistributionsToRawSequences[Sym, Tag](
    rawSequences: Seq[IndexedSeq[Sym]],
    tagDict: OptionalTagDict[Sym, Tag],
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]): Seq[IndexedSeq[(Option[Sym], Vector[(Option[Tag], (Map[Option[Tag], LogNum], LogNum))])]] = {

    val allTags = tagDict.allTags + None
    val reverseTransitions =
      allTags
        .mapTo { prevTag =>
          val transitionsFromPrev = transitions(prevTag)
          allTags.mapTo(transitionsFromPrev)
        }
        .ungroup
        .map { case (prevTag, (currTag, p)) => currTag -> (prevTag -> p) }
        .groupByKey
        .mapVals(_.toMap)

    rawSequences.map(_.ended.map { sym =>
      val tags = tagDict.set(sym).toVector
      sym -> tags.mapTo { currTag =>
        (reverseTransitions(currTag), emissions(currTag)(sym))
      }
    })

  }

}
