package opennlp.scalabha.tag.support

import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.tag._
import scala.annotation.tailrec
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }

/**
 * A generic implementation of the Viterbi algorithm.
 *
 * @param edgeScorer	Class for calculating the probability of a symbol/tag transition
 */
class Viterbi[Sym, Tag](edgeScorer: TagEdgeScorer[Sym, Tag]) {

  /**
   * Find the most likely tagging for the sequence given no constraints on
   * which tags can be associated with which symbols.
   *
   * @param sequence	A sequence to be tagged.
   * @param tagSet		A set of all tags
   */
  def tagSequence(sequence: IndexedSeq[Sym], tagSet: Set[Tag]): List[Tag] = {
    tagSequence(sequence, OptionalTagDict(SimpleTagDict(Map[Sym, Set[Tag]](), tagSet)))
  }

  /**
   * Find the most likely tagging for the sequence.
   *
   * @param sequence	A sequence to be tagged.
   * @param tagDict		Tag dictionary indicating which words can be used with which tags.
   */
  def tagSequence(sequence: IndexedSeq[Sym], tagDict: OptionalTagDict[Sym, Tag]): List[Tag] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Set the initial values for the fold based on the initial observation
    val startViterbi = Map[Option[Tag], LogNum](None -> LogNum.one)
    val startBackpointers = List[Map[Option[Tag], Option[Tag]]]()
    val startSymbol: Option[Sym] = None

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val (lastViterbi, backpointers, _) =
      (sequence.map(Some(_)) :+ None).foldLeft((startViterbi, startBackpointers, startSymbol)) {
        case ((viterbi, backpointers, prevSym), currSym) =>
          // for each possible tag, get the highest probability previous tag and its score
          val transitionScores =
            tagDict.set(currSym).mapTo(currTag => // each legal tag for the current symbol
              viterbi.map {
                case (prevTag, viterbtiScore) =>
                  (prevTag, viterbtiScore * edgeScorer(prevSym, prevTag, currSym, currTag))
              })
              .toMap
          val bestTransitions = transitionScores.mapValuesStrict(_.maxBy(_._2)) // get the previous tag with the highest probability (and its score)
          (bestTransitions.mapValuesStrict(_._2), // update viterbi for the next row
            bestTransitions.mapValuesStrict(_._1) :: backpointers, // append new backpointers
            currSym)
      }

    // Get the optimal tag sequence and map the tag indices back to their string values
    backtrack(backpointers).flatten
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[Option[Tag], Option[Tag]]]): List[Option[Tag]] = {
    @tailrec def inner(backpointers: List[Map[Option[Tag], Option[Tag]]], curTag: Option[Tag], tags: List[Option[Tag]]): List[Option[Tag]] =
      backpointers match {
        case Nil => assert(curTag == None); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag :: tags)
      }
    val Pattern.Map(None -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil)
  }
}

////////////////////////////////
// TagEdgeScorer
////////////////////////////////

trait TagEdgeScorer[Sym, Tag] {
  /**
   * Calculate the value of a transition from the previous word/tag pair to the current word/tag pair.
   */
  def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]): LogNum
}
