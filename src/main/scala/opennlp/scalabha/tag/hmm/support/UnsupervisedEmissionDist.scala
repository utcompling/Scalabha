package opennlp.scalabha.tag.hmm.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag.support._
import org.apache.commons.logging.LogFactory

/**
 * Produce a conditional frequency distribution without labeled training data.
 */
trait UnsupervisedEmissionDistFactory[Tag, Sym] {
  def make(): Tag => Sym => LogNum
}

/**
 * For each word, assume a word/tag count of 1 for each entry in the tag
 * dictionary.  This gives an even distribution across all words known to have
 * the tag.  Then, assume one additional count that is divided across all
 * other words in the vocabulary.  However, since our HMM does not allow
 * known words to be associated with tags outside of the tag dictionary,
 * these are simply zero, but that same count is used for unseen words.
 * Thus, the 'default' count for a tag is 1/(V-S) where V is the
 * vocabulary size and S is the number of words associated with the tag.
 *
 * In the implementation, for a tag, we give a count of (V-S) to each word in
 * the tag dictionary associated with that tag.  We also add an extra (V-S) to
 * the total count for normalizing.  Unseen words have a 'default' count of 1,
 * yielding a probability of 1/((S+1)*(V-S)).
 */
class OneCountUnsupervisedEmissionDistFactory[Tag, Sym](tagDict: Map[Sym, Set[Tag]], lambda: Double, startEndSymbol: Sym, startEndTag: Tag)
  extends UnsupervisedEmissionDistFactory[Tag, Sym] {

  override def make(): Tag => Sym => LogNum = {
    val symbolsForTag = (tagDict.flattenOver.map(_.swap).toSet.groupByKey - startEndTag).mapValuesStrict(_ - startEndSymbol)
    val totalNumSymbols = (tagDict.keySet - startEndSymbol).size
    val counts =
      symbolsForTag.mapValuesStrict {
        symbols =>
          val numInvalidSymbols = totalNumSymbols - symbols.size
          DefaultedFreqCounts(symbols.mapTo(s => countForSym(s) * numInvalidSymbols).toMap, lambda * numInvalidSymbols, lambda)
      } + (startEndTag -> DefaultedFreqCounts(Map(startEndSymbol -> 2 * lambda), 0.0, 0.0))
    CondFreqDist(new DefaultedCondFreqCounts(counts))
  }

  protected def countForSym(sym: Sym): Double = {
    lambda
  }
}

/**
 * For each word, assume a word/tag count of 1/T where T is the number of tags
 * with which the word is associated in the tag dictionary.
 * This gives a distribution across all words known to have the tag that
 * assigns lower weight to the words that are divided among many tags.
 * Then, assume one additional count that is divided across all
 * other words in the vocabulary.  However, since our HMM does not allow
 * known words to be associated with tags outside of the tag dictionary,
 * these are simply zero, but that same count is used for unseen words.
 * Thus, the 'default' count for a tag is 1/(V-S) where V is the
 * vocabulary size and S is the number of words associated with the tag.
 *
 * In the implementation, for a tag, we give a count of (V-S)/T to each word in
 * the tag dictionary associated with that tag.  We also add an extra (V-S) to
 * the total count for normalizing.  Unseen words have a 'default' count of 1.
 */
class PartialCountUnsupervisedEmissionDistFactory[Tag, Sym](tagDict: Map[Sym, Set[Tag]], lambda: Double, startEndSymbol: Sym, startEndTag: Tag)
  extends OneCountUnsupervisedEmissionDistFactory[Tag, Sym](tagDict, lambda, startEndSymbol, startEndTag) {

  override protected def countForSym(sym: Sym): Double = {
    lambda / tagDict(sym).size
  }
}

/**
 * Count occurrences of each word in the raw data and spread them evenly
 * among the tags to which the word is associated in the tag dictionary.
 * To smooth, add one to each count, and assume count of one for words in
 * tag dictionary not seen in raw data.
 *
 * So, if a word w appears C(w) times in the raw corpus, and is seen with
 * |TD(w)| tags in the tag dictionary, then assume
 * C(t,w) = (C(w) + 1) / |TD(w)|.
 *
 * Known words not associated with the tag in the tag dictionary are given
 * zero counts.  Unseen words are assumed to have a 'default' count of 1 for
 * each tag.
 */
class EstimatedRawCountUnsupervisedEmissionDistFactory[Tag, Sym](tagDict: Map[Sym, Set[Tag]], rawData: Iterable[Iterable[Sym]], lambda: Double, startEndSymbol: Sym, startEndTag: Tag)
  extends UnsupervisedEmissionDistFactory[Tag, Sym] {
  protected val LOG = LogFactory.getLog(classOf[EstimatedRawCountUnsupervisedEmissionDistFactory[Tag, Sym]])

  override def make(): Tag => Sym => LogNum = {
    val symbolCounts = (rawData.flatten.counts - startEndSymbol).withDefaultValue(0)
    val symbolsForTag = (tagDict.flattenOver.map(_.swap).toSet.groupByKey - startEndTag).mapValuesStrict(_ - startEndSymbol)
    val counts =
      symbolsForTag.mapValuesStrict {
        symbols =>
          DefaultedFreqCounts(symbols.mapTo(s => (symbolCounts(s) + lambda) / tagDict(s).size.toDouble).toMap, lambda, lambda)
      } + (startEndTag -> DefaultedFreqCounts(Map(startEndSymbol -> 2 * lambda), 0.0, 0.0))
    CondFreqDist(new DefaultedCondFreqCounts(counts))
  }
}

/**
 * Hack the default probability for each tag by making it |TD(tag)| / V
 */
class DefaultHackingUnsupervisedEmissionDistFactory[Tag, Sym](tagDict: Map[Sym, Set[Tag]], startEndSymbol: Sym, startEndTag: Tag, delegate: UnsupervisedEmissionDistFactory[Tag, Sym])
  extends UnsupervisedEmissionDistFactory[Tag, Sym] {
  protected val LOG = LogFactory.getLog(classOf[DefaultHackingUnsupervisedEmissionDistFactory[Tag, Sym]])

  override def make(): Tag => Sym => LogNum = {
    val dist = delegate.make()
    val distMap = dist.asInstanceOf[Map[Tag, Map[Sym, LogNum]]]
    val totalNumSymbols = (tagDict.keySet - startEndSymbol).size.toDouble

    val defaultDist = FreqDist(DefaultedFreqCounts(distMap.map { case (tag, symbols) => tag -> math.log(symbols.size) }, 0.0, 0.0))

    distMap.toList.sortBy(_._2.size).map {
      case (tag, symbols) =>
        LOG.debug("%4s: \ttotalNumSymbols = %s, symbols.size = %d".format(tag, totalNumSymbols, symbols.size))
        tag -> symbols.withDefaultValue(defaultDist(tag))
    }.toMap.withDefaultValue(FreqDist.empty)
  }

}
