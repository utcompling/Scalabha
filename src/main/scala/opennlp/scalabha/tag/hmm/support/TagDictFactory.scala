package opennlp.scalabha.tag.hmm.support

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
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 */
class SimpleTagDictFactory[Sym, Tag]() extends TagDictFactory[Sym, Tag] {
  def make(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    val (symSequences, tagSequences) = taggedTrainSequences.map(_.unzip).unzip
    val symTagPairs = (symSequences.flatten zipEqual tagSequences.flatten)

    // Construct the tag dictionary (words to their known possible tags).
    // By default, map any unseen words to all known tags since, if we don't know anything 
    // about the word, then we must assume it can be anything.
    val tagDict = symTagPairs.toSet.groupByKey
    val allTags = tagDict.flatMap(_._2).toSet
    tagDict.withDefaultValue(allTags)
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
