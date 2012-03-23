package opennlp.scalabha.tag.hmm

import scala.annotation.tailrec
import scala.io.Source
import opennlp.scalabha.tag._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern.{ +:, :+ }
import opennlp.scalabha.tag.SupervisedTaggerTrainer
import opennlp.scalabha.tag.UnsupervisedTaggerTrainer
import opennlp.scalabha.tag.hmm.support._
import org.apache.commons.logging.LogFactory
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

/**
 * Factory for training a Hidden Markov Model tagger directly from labeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitionCountsTransformer	factory for generating builders that count tag occurrences and compute distributions
 * @param emissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions
 * @param startEndSymbol				a unique start/end symbol used internally to mark the beginning and end of a sentence
 * @param startEndTag					a unique start/end tag used internally to mark the beginning and end of a sentence
 */
class SupervisedHmmTaggerTrainer[Sym, Tag](
  transitionCountsTransformer: CondCountsTransformer[Tag, Tag],
  emissionCountsTransformer: CondCountsTransformer[Tag, Sym],
  startEndSymbol: Sym,
  startEndTag: Tag)
  extends SupervisedTaggerTrainer[Sym, Tag] {

  private val LOG = LogFactory.getLog(classOf[SupervisedHmmTaggerTrainer[Sym, Tag]]);

  /**
   * Train a Hidden Markov Model tagger directly from labeled data.
   *
   * Main features:
   * <ul>
   *   <li> Constructs a tag dictionary directly from labeled data using tagDictFactory.
   *   <li> Uses transition and emission counters to compute distributions based on labeled data.
   * </ul>
   *
   * @param taggedTrainSequences	labeled sequences to use for training the model
   * @param tagDict					tag dictionary
   * @return						a trained tagger
   */
  override def trainSupervised(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]], tagDict: Map[Sym, Set[Tag]]): Tagger[Sym, Tag] = {
    val (transitionCounts, emissionCounts) = getCountsFromTagged(taggedTrainSequences)
    val transitionDist = CondFreqDist(transitionCountsTransformer(transitionCounts))
    val emissionDist = CondFreqDist(emissionCountsTransformer(emissionCounts))
    new HmmTagger(transitionDist, emissionDist, tagDict + (startEndSymbol -> Set(startEndTag)), startEndSymbol, startEndTag)
  }

  /**
   * Get transition and emission counts from labeled data
   *
   * @param taggedTrainSequences	labeled sequences from which to extract counts
   */
  protected def getCountsFromTagged(taggedSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    // Separate symbols from tags.  Add start/final symbols and tags to each sequence
    val endedSequences = taggedSequences.map(((startEndSymbol -> startEndTag) +: _ :+ (startEndSymbol -> startEndTag)))

    // Get the tag transitions, including start/final tags
    val tagPairs = endedSequences.map(_.map(_._2).sliding2).flatten
    val transitionCounts = tagPairs.groupByKey.mapValuesStrict(_.counts)

    // Get the word/tag pairs (emissions)
    val tagSymbolPairs = endedSequences.flatMap(_.map(_.swap))
    val emissionCounts = tagSymbolPairs.groupByKey.mapValuesStrict(_.counts)

    (transitionCounts, emissionCounts)
  }
}
