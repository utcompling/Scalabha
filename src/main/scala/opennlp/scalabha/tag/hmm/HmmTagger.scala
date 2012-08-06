package opennlp.scalabha.tag.hmm

import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.support.TagEdgeScorer
import opennlp.scalabha.tag.support.Viterbi
import opennlp.scalabha.util.LogNum

/**
 * Hidden Markov Model for tagging.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitions		function giving the conditional probability of a tag given its previous tag
 * @param emissions			function giving the probability of a symbol given a tag
 * @param tagSet			the set of all possible tags
 *
 * NOTE: Start and end symbols and tags are represented by None.
 */
case class HmmTagger[Sym, Tag](
  val transitions: Option[Tag] => Option[Tag] => LogNum,
  val emissions: Option[Tag] => Option[Sym] => LogNum,
  val tagSet: Set[Tag])
  extends Tagger[Sym, Tag] {

  protected[this] val viterbi = new Viterbi(new HmmEdgeScorer(transitions, emissions), tagSet)

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override protected def tagSequence(sequence: IndexedSeq[Sym]) =
    viterbi.tagSequence(sequence)

}

/**
 * Hidden Markov Model for tagging that has hard constraints on transitions
 * and/or emissions.  While this is functionally equivalent to using
 * constrained counts transformers, this class will be faster since Viterbi
 * won't bother exploring those zero-probability values.
 */
class HardConstraintHmmTagger[Sym, Tag](
  transitions: Option[Tag] => Option[Tag] => LogNum,
  emissions: Option[Tag] => Option[Sym] => LogNum,
  tagSet: Set[Tag],
  override protected[this] val viterbi: Viterbi[Sym, Tag])
  extends HmmTagger[Sym, Tag](transitions, emissions, tagSet) {

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagDict: OptionalTagDict[Sym, Tag]) =
    this(transitions, emissions, tagDict.unoptioned.allTags, new Viterbi(new HmmEdgeScorer(transitions, emissions), tagDict))

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagSet: Set[Tag],
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    this(transitions, emissions, tagSet, new Viterbi(new HmmEdgeScorer(transitions, emissions), tagSet, validTransitions))

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagDict: OptionalTagDict[Sym, Tag],
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    this(transitions, emissions, tagDict.unoptioned.allTags, new Viterbi(new HmmEdgeScorer(transitions, emissions), tagDict, validTransitions))

}

object HardConstraintHmmTagger {
  def apply[Sym, Tag](
    hmmTagger: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag], allowUnseenWordTypes: Boolean) =
    new HardConstraintHmmTagger(hmmTagger.transitions, hmmTagger.emissions, tagDict.unoptioned.allTags,
      new Viterbi(new HmmEdgeScorer(hmmTagger.transitions, hmmTagger.emissions), tagDict))

  def apply[Sym, Tag](
    hmmTagger: HmmTagger[Sym, Tag],
    tagSet: Set[Tag],
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    new HardConstraintHmmTagger(hmmTagger.transitions, hmmTagger.emissions, tagSet,
      new Viterbi(new HmmEdgeScorer(hmmTagger.transitions, hmmTagger.emissions), tagSet, validTransitions))

  def apply[Sym, Tag](
    hmmTagger: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag], allowUnseenWordTypes: Boolean,
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    new HardConstraintHmmTagger(hmmTagger.transitions, hmmTagger.emissions, tagDict.unoptioned.allTags,
      new Viterbi(new HmmEdgeScorer(hmmTagger.transitions, hmmTagger.emissions), tagDict, validTransitions))
}

//////////////////////////////
// HmmTagger Factories
//////////////////////////////

trait HmmTaggerFactory[Sym, Tag] {
  def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag]
}

class SimpleHmmTaggerFactory[Sym, Tag](tagSet: Set[Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag] = {
    HmmTagger(transitions, emissions, tagSet)
  }
}

class HardTagDictConstraintHmmTaggerFactory[Sym, Tag](tagDict: OptionalTagDict[Sym, Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum): HmmTagger[Sym, Tag] = {
    new HardConstraintHmmTagger(transitions, emissions, tagDict)
  }
}

//////////////////////////////
// Edge Scorer
//////////////////////////////

/**
 * Edge Scorer used for Viterbi HMM tagging
 */
class HmmEdgeScorer[Sym, Tag](
  transitions: Option[Tag] => Option[Tag] => LogNum,
  emissions: Option[Tag] => Option[Sym] => LogNum)
  extends TagEdgeScorer[Sym, Tag] {

  override def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]): LogNum = {
    val t = transitions(prevTag)(currTag) // probability of transition to current
    val e = emissions(currTag)(currSym) // probability of observing current symbol
    t * e
  }
}
