package opennlp.scalabha.tag.hmm

import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.TagDict.OptionalTagDict

object HmmUtils {

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  def getCountsFromTagged[Sym, Tag](taggedSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    // Separate symbols from tags.  Add start/final symbols and tags to each sequence
    val endedSequences = taggedSequences.map(((None -> None) +: _.map { case (s, t) => Some(s) -> Some(t) } :+ (None -> None)))

    // Get the tag transitions, including start/final tags
    val tagPairs = endedSequences.map(_.map(_._2).sliding2).flatten
    val transitionCounts = tagPairs.groupByKey.mapVals(_.counts)

    // Get the word/tag pairs (emissions)
    val tagSymbolPairs = endedSequences.flatMap(_.map(_.swap))
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

  def makeTransitionCounts[Sym, Tag, N: Numeric](tagDict: OptionalTagDict[Sym, Tag], countMaps: (Option[Tag] => Option[Tag] => N)*) = {
    val allTags = tagDict.allTags + None
    allTags.mapTo(tag1 => allTags.mapTo(tag2 => countMaps.sumBy(_(tag1)(tag2))).toMap).toMap
  }

  def makeEmissionCounts[Sym, Tag, N: Numeric](tagDict: OptionalTagDict[Sym, Tag], countMaps: (Option[Tag] => Option[Sym] => N)*) = {
    val allTags = tagDict.allTags + None
    val allSyms = tagDict.symbols + None
    allTags.mapTo(tag => allSyms.mapTo(sym => countMaps.sumBy(_(tag)(sym))).toMap).toMap
  }

}
