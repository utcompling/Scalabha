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
  val tagDict: OptionalTagDict[Sym, Tag])
  extends Tagger[Sym, Tag] {

  private[this] val viterbi =
    new Viterbi(new TagEdgeScorer[Sym, Tag] {
      override def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]): LogNum = {
        val t = transitions(prevTag)(currTag) // probability of transition to current
        val e = emissions(currTag)(currSym) // probability of observing current symbol
        t * e
      }
    })

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]): List[Tag] = {
    viterbi.tagSequence(sequence, tagDict).getOrElse(
      throw new RuntimeException("No tagging found for '%s'".format(sequence.mkString(" "))))
  }

}
