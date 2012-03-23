package opennlp.scalabha.tag.hmm

import scala.annotation.tailrec

import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.LogNum._
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
 * @param startEndSymbol	the special start/end symbol used in the transitions and emissions to mark
 * 							the beginning and end of a sentence
 * @param startEndTag		the special start/end tag used in the transitions and emissions to mark
 * 							the beginning and end of a sentence
 */
case class HmmTagger[Sym, Tag](
  val transitions: Tag => Tag => LogNum,
  val emissions: Tag => Sym => LogNum,
  val tagDict: Map[Sym, Set[Tag]],
  val startEndSymbol: Sym,
  val startEndTag: Tag)
  extends Tagger[Sym, Tag] {

  /**
   * Tag the sequence using this model.  Uses the Viterbi algorithm.
   *
   * @param sequence 	a single sequence to be tagged
   * @return			the tagging of the input sequence assigned by the model
   */
  override def tagSequence(sequence: IndexedSeq[Sym]): List[Tag] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Make sure sequence and tag dictionary use startEndSymbol 
    val tagDictWithEnds = tagDict + (startEndSymbol -> Set(startEndTag))

    // Get initial viterbi for start symbol
    val startViterbi = Map(startEndTag -> LogNum.one)

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val (lastViterbi, backpointers) =
      (sequence :+ startEndSymbol).foldLeft((startViterbi, List[Map[Tag, Tag]]())) {
        case ((viterbi, backpointers), tok) =>
          // for each possible tag, get the highest probability previous tag and its score
          val transitionScores =
            tagDictWithEnds(tok).mapTo(currTag => // each legal tag for the current token
              viterbi.map {
                case (prevTag, viterbtiScore) =>
                  val v = viterbtiScore // probability of the most likely sequence of states leading to prevTag
                  val t = transitions(prevTag)(currTag) // probability of transition to current
                  val e = emissions(currTag)(tok) // probability of observation from current
                  (prevTag, v * t * e)
              })
              .toMap
          val bestTransitions = transitionScores.mapValuesStrict(_.maxBy(_._2)) // get the previous tag with the highest probability (and its score)
          (bestTransitions.mapValuesStrict(_._2), // update viterbi for the next row
            bestTransitions.mapValuesStrict(_._1) :: backpointers) // append new backpointers
      }

    // Get the optimal tag sequence and map the tag indices back to their string values
    backtrack(backpointers)
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[Tag, Tag]]): List[Tag] = {
    @tailrec def inner(backpointers: List[Map[Tag, Tag]], curTag: Tag, tags: List[Tag]): List[Tag] =
      backpointers match {
        case Nil => assert(curTag == startEndTag); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag :: tags)
      }
    val Pattern.Map(`startEndTag` -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil)
  }
}
