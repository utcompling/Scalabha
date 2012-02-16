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
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

/**
 * Factory for training a Hidden Markov Model tagger directly from labeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitionCounterFactory	factory for generating builders that count tag occurrences and compute distributions
 * @param emissionCounterFactory	factory for generating builders that count symbol occurrences and compute distributions
 * @param startEndSymbol			a unique start/end symbol used internally to mark the beginning and end of a sentence
 * @param startEndTag				a unique start/end tag used internally to mark the beginning and end of a sentence
 * @param tagDictFactory			factory for generating tag dictionary from labeled data
 */
class SupervisedHmmTaggerTrainer[Sym, Tag](
  transitionCounterFactory: CondFreqCounterFactory[Tag, Tag],
  emissionCounterFactory: CondFreqCounterFactory[Tag, Sym],
  startEndSymbol: Sym,
  startEndTag: Tag,
  tagDictFactory: TagDictFactory[Sym, Tag] = new SimpleTagDictFactory[Sym, Tag]())
  extends SupervisedTaggerTrainer[Sym, Tag] {

  private val LOG = LogFactory.getLog(SupervisedHmmTaggerTrainer.getClass);

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
   * @return						a trained tagger
   */
  override def trainSupervised(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {
    val tagDict = tagDictFactory.make(taggedTrainSequences)
    val (transitions, emissions) = getCountsFromTagged(taggedTrainSequences)
    new HmmTagger(transitions.toFreqDist, emissions.toFreqDist, tagDict, startEndSymbol, startEndTag)
  }

  /**
   * Get transition and emission counts from labeled data
   */
  protected def getCountsFromTagged(taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]) = {
    // Separate symbols from tags.  Add start/final symbols and tags to each sequence
    val (symSequences, tagSequences) = taggedTrainSequences.map(
      seq => ((startEndSymbol -> startEndTag) +: seq :+ (startEndSymbol -> startEndTag))
        .unzip).unzip

    // Get the tag transitions, including start/final tags
    val tagPairs = tagSequences.map(_.sliding2).flatten
    val transitionCounter = transitionCounterFactory.get ++= tagPairs

    // Get the word/tag pairs (emissions)
    val tagSymbolPairs = (tagSequences.flatten zipEqual symSequences.flatten)
    val emissionCounter = emissionCounterFactory.get ++= tagSymbolPairs

    (transitionCounter, emissionCounter)
  }
}
