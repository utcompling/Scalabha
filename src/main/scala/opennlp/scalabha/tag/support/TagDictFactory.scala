package opennlp.scalabha.tag.support

import opennlp.scalabha.tag.SimpleTagDict
import opennlp.scalabha.tag.SimpleWeightedTagDict
import opennlp.scalabha.tag.TagDict
import opennlp.scalabha.tag.TagDict._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._

/**
 * Factory for creating a tag dictionary (mapping from symbols to valid tags)
 * from labeled data.
 *
 * This is the top of a hierarchy is designed for modular approach to tag
 * dictionary creation.  The SimpleTagDictFactory is intended as the basic
 * factory that simply finds all tags for each word.  Other
 * TagDictFactory implementations should wrap SimpleTagDictFactory and, in
 * their versions of makeTagDictFromTagged, transform the results of the
 * lower-level TagDictFactory.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
trait TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): TagDict[Sym, Tag]
}

/**
 * Basic TagDictFactory that maps each symbol of the labeled data to
 * the set of all tags with which it is seen in association.
 * Default for unknown symbols is the set of all seen tags.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
class SimpleTagDictFactory[Sym, Tag]() extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    SimpleTagDict(taggedTrainSequences.flatten.toSet.groupByKey)
  }
}

/**
 * Construct a tag dictionary from a labeled corpus.  Each weight value is 
 * given by P(tag | sym).  For unseen symbols, the weight is simply P(tag).
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
class SimpleWeightedTagDictFactory[Sym, Tag](condCountsTransformer: CondCountsTransformer[Sym, Tag]) extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    val counts = taggedTrainSequences.flatten.groupByKey.mapVals(_.counts.mapVals(_.toLogNum))
    val CondFreqDist(dists, default) = CondFreqDist(condCountsTransformer(counts))
    SimpleWeightedTagDict(dists.mapVals(_.dist), default.dist)
  }
}

/**
 * TagDictFactory that takes only the top N most frequent symbol/tag pairs.
 * Default for unknown symbols is the set of tags passed in.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param numSymTagPairs	the maximum number of top symbol/tag pairs to count
 */
class TopSymTagPairTagDictFactory[Sym, Tag](numSymTagPairs: Int) extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    val wordTagPairCounts = taggedTrainSequences.flatten.counts
    val topWordTagPairs = wordTagPairCounts.toVector.sortBy(-_._2).map(_._1).take(numSymTagPairs)
    val tagDict = topWordTagPairs.toSet.groupByKey
    val fullTagset = wordTagPairCounts.keySet.map(_._2)
    SimpleTagDict(tagDict, fullTagset)
  }
}

/**
 * TagDictFactory that extracts a full tagset for the top N most frequent symbols.
 * Default for unknown symbols is the set of tags passed in.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param numSym	the maximum number of top symbols to use
 */
class FullTopSymTagDictFactory[Sym, Tag](numSym: Int, fullTagset: Set[Tag]) extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    val wordCounts = taggedTrainSequences.flatten.map(_._1).counts
    val topWords = wordCounts.toVector.sortBy(-_._2).map(_._1).take(numSym)
    val fullTagDict = taggedTrainSequences.flatten.toSet.groupByKey
    val tagDict = topWords.mapTo(fullTagDict).toMap
    SimpleTagDict(tagDict, fullTagset)
  }
}

/**
 * Limit the delegate-produced dictionary to the top 'maxNumberOfDefaultTags'
 * tags as ordered by the number of symbols with which the tag is associated.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param maxNumberOfDefaultTags	maximum number of tags to be available to unseen symbols
 */
class DefaultLimitingTagDictFactory[Sym, Tag](maxNumberOfDefaultTags: Int, delegate: TagDictFactory[Sym, Tag]) extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    val delegateDict = delegate.make(taggedTrainSequences).setIterator.toMap
    val topNTags =
      delegateDict
        .ungroup
        .map(_.swap).toSet
        .groupByKey
        .mapVals(_.size).toVector
        .sortBy(-_._2)
        .take(maxNumberOfDefaultTags)
        .map(_._1).toSet
    SimpleTagDict(delegateDict, topNTags)
  }
}

class ExternalFileTagDictFactory(filename: String, fullTagset: Set[String]) extends TagDictFactory[String, String] {
  override def make(taggedTrainSequences: Iterable[IndexedSeq[(String, String)]]) = {
    SimpleTagDict(io.Source.fromFile(filename).getLines
      .map(_.split("\\s+"))
      .flatMap { case Array(word, tags @ _*) => tags.map(word -> _) }
      .groupByKey
      .mapVals(_.toSet),
      fullTagset)
  }
}
