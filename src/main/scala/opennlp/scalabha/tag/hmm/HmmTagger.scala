package opennlp.scalabha.tag.hmm

import opennlp.scalabha.tag.support.TagEdgeScorer
import opennlp.scalabha.tag.support.Viterbi
import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.CollectionUtils._
import scala.annotation.tailrec

/**
 * Hidden Markov Model for tagging.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitions		function giving the conditional probability of a tag given its previous tag
 * @param emissions			function giving the probability of a symbol given a tag
 * @param tagDict			tag dictionary representing the valid tags for a given symbol
 *
 * NOTE: Start and end symbols and tags are represented by None.
 */
case class HmmTagger[Sym, Tag](
  val transitions: Option[Tag] => Option[Tag] => LogNum,
  val emissions: Option[Tag] => Option[Sym] => LogNum,
  val tagSet: Set[Tag])
  extends Tagger[Sym, Tag] {

  private[this] val viterbi = new Viterbi(new HmmEdgeScorer(transitions, emissions), tagSet)

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]) =
    viterbi.tagSequence(sequence)

}

/**
 * Hidden Markov Model for tagging that has hard constraints on transitions
 * and/or emissions.  While this is functionally equivalent to using
 * constrained counts transformers, this class will be faster since Viterbi
 * won't bother exploring those zero-probability values.
 */
class HardConstraintHmmTagger[Sym, Tag](
  val viterbi: Viterbi[Sym, Tag])
  extends Tagger[Sym, Tag] {

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagSet: Set[Tag]) =
    this(new Viterbi(new HmmEdgeScorer(transitions, emissions), tagSet))

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagDict: OptionalTagDict[Sym, Tag]) =
    this(new Viterbi(new HmmEdgeScorer(transitions, emissions), tagDict))

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagSet: Set[Tag],
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    this(new Viterbi(new HmmEdgeScorer(transitions, emissions), tagSet, validTransitions))

  def this(
    transitions: Option[Tag] => Option[Tag] => LogNum,
    emissions: Option[Tag] => Option[Sym] => LogNum,
    tagDict: OptionalTagDict[Sym, Tag],
    validTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    this(new Viterbi(new HmmEdgeScorer(transitions, emissions), tagDict, validTransitions))

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]) =
    viterbi.tagSequence(sequence)

}

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
