package opennlp.scalabha.tag.hmm

import opennlp.scalabha.tag.support.TagEdgeScorer
import opennlp.scalabha.tag.support.Viterbi
import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.util.LogNum

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

  private[this] val viterbi = new Viterbi(new HmmEdgeScorer(transitions, emissions))

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]): List[Tag] = {
    viterbi.tagSequence(sequence, tagSet).getOrElse(
      throw new RuntimeException("No tagging found for '%s'".format(sequence.mkString(" "))))
  }

}

/**
 * Hidden Markov Model for tagging that has hard constraints on transitions
 * and/or emissions.  While this is functionally equivalent to using
 * constrained counts transformers, this class will be faster since Viterbi
 * won't bother exploring those zero-probability values.
 */
case class HardConstraintHmmTagger[Sym, Tag](
  val transitions: Option[Tag] => Option[Tag] => LogNum,
  val emissions: Option[Tag] => Option[Sym] => LogNum,
  val tagDict: Option[OptionalTagDict[Sym, Tag]],
  val tagSet: Option[Set[Tag]],
  val validTransitions: Option[Map[Option[Tag], Set[Option[Tag]]]])
  extends Tagger[Sym, Tag] {

  private[this] val viterbi = new Viterbi(new HmmEdgeScorer(transitions, emissions))

  private[this] val tag = (sequence: Seq[Sym] @unchecked) =>
    (tagDict, tagSet, validTransitions) match {
      case (Some(e), None, None) => viterbi.tagSequence(sequence, e)
      case (Some(e), None, Some(t)) => viterbi.tagSequence(sequence, e, t)
      case (None, Some(s), Some(t)) => viterbi.tagSequence(sequence, s, t)
      case (None, Some(s), None) => viterbi.tagSequence(sequence, s)
    }

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]): List[Tag] = {
    tag(sequence).getOrElse(throw new RuntimeException("No tagging found for '%s'".format(sequence.mkString(" "))))
  }
}

object HardConstraintHmmTagger {
  def apply[Sym, Tag](
    hmmTagger: HmmTagger[Sym, Tag],
    tagDict: Option[OptionalTagDict[Sym, Tag]],
    tagSet: Option[Set[Tag]],
    validTransitions: Option[Map[Option[Tag], Set[Option[Tag]]]]) = {
    new HardConstraintHmmTagger(hmmTagger.transitions, hmmTagger.emissions, tagDict, tagSet, validTransitions)
  }
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
    //        if (t > LogNum.zero && e > LogNum.zero)
    //          println("%s\t%s\t(%s %s)\t(%s %s)".format(t, e, prevSym, prevTag, currSym, currTag))
    t * e
  }
}
