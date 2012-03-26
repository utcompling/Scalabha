package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._

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
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Map[Sym, Set[Tag]]
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
    val tagDict = taggedTrainSequences.flatten.toSet.groupByKey
    val allTags = tagDict.flatMap(_._2).toSet
    tagDict.withDefaultValue(allTags)
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
    val topWordTagPairs = wordTagPairCounts.toList.sortBy(-_._2).map(_._1).take(numSymTagPairs)
    val tagDict = topWordTagPairs.toSet.groupByKey
    val fullTagset = wordTagPairCounts.keySet.map(_._2)
    tagDict.withDefaultValue(fullTagset)
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
    val topWords = wordCounts.toList.sortBy(-_._2).map(_._1).take(numSym)
    val fullTagDict = taggedTrainSequences.flatten.toSet.groupByKey
    val tagDict = topWords.mapTo(fullTagDict).toMap
    tagDict.withDefaultValue(fullTagset)
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
    val delegateDict = delegate.make(taggedTrainSequences)
    val topNTags =
      delegateDict
        .flattenOver
        .map(_.swap).toSet
        .groupByKey
        .mapValuesStrict(_.size).toList
        .sortBy(-_._2)
        .take(maxNumberOfDefaultTags)
        .map(_._1).toSet
    delegateDict.withDefaultValue(topNTags)
  }
}

class ExternalFileTagDictFactory(filename: String, fullTagset: Set[String]) extends TagDictFactory[String, String] {
  override def make(taggedTrainSequences: Iterable[IndexedSeq[(String, String)]]) = {
    io.Source.fromFile(filename).getLines
      .map(_.split("\\s+"))
      .flatMap { case Array(word, tags @ _*) => tags.map(word -> _) }
      .groupByKey
      .mapValuesStrict(_.toSet)
      .withDefaultValue(fullTagset)
  }
}
