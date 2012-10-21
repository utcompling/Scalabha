package opennlp.scalabha.tag.support

import scala.annotation.tailrec
import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.SimpleTagDict
import opennlp.scalabha.tag.TagDict._
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }

/**
 * A generic implementation of the Viterbi algorithm for finding the most
 * likely tagging for the sequence.
 *
 * @param edgeScorer		class for calculating the probability of a symbol/tag transition
 * @param tagDict			tag dictionary indicating which words can be used with which tags
 * @param tagTransitions	valid tag-tag transitions
 */
class Viterbi[Sym, Tag](
  edgeScorer: TagEdgeScorer[Sym, Tag],
  tagDict: OptionalTagDict[Sym, Tag],
  tagTransitions: Map[Option[Tag], Set[Option[Tag]]])
  extends Tagger[Sym, Tag] {

  /**
   * Find the most likely tagging for the sequence given no constraints on
   * tag-tag transitions
   *
   * @param sequence		sequence to be tagged
   * @param tagDict			tag dictionary indicating which words can be used with which tags
   */
  def this(edgeScorer: TagEdgeScorer[Sym, Tag], tagDict: OptionalTagDict[Sym, Tag]) =
    this(edgeScorer, tagDict, { val allTags = tagDict.allTags + None; allTags.mapToVal(allTags).toMap })

  /**
   * Find the most likely tagging for the sequence given no constraints on
   * which tags can be associated with which symbols
   *
   * @param sequence		sequence to be tagged
   * @param tagSet			set of all tags
   * @param tagTransitions	valid tag-tag transitions
   */
  def this(edgeScorer: TagEdgeScorer[Sym, Tag], tagSet: Set[Tag], tagTransitions: Map[Option[Tag], Set[Option[Tag]]]) =
    this(edgeScorer, OptionalTagDict(SimpleTagDict(Map[Sym, Set[Tag]](), tagSet)), tagTransitions)

  /**
   * Find the most likely tagging for the sequence given no constraints on
   * which tags can be associated with which symbols or tag-tag transitions
   *
   * @param sequence		sequence to be tagged
   * @param tagSet			set of all tags
   */
  def this(edgeScorer: TagEdgeScorer[Sym, Tag], tagSet: Set[Tag]) =
    this(edgeScorer, OptionalTagDict(SimpleTagDict(Map[Sym, Set[Tag]](), tagSet)))

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  override def tagOptions(rawSequences: Vector[Vector[Sym]]): Vector[Option[Vector[(Sym, Tag)]]] =
    rawSequences.par.map(tagSequence).seq

  /**
   * Find the most likely tagging for the sequence.
   *
   * @param sequence		sequence to be tagged
   */
  override def tagSequence(sequence: Vector[Sym]): Option[Vector[(Sym, Tag)]] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Set the initial values for the fold based on the initial observation
    val startViterbi = Map[Option[Tag], LogNum](None -> LogNum.one)
    val startBackpointers = List[Map[Option[Tag], Option[Tag]]]()
    val startSymbol: Option[Sym] = None

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val backpointers =
      (sequence.map(Some(_)) :+ None).foldLeft(Option(startViterbi, startBackpointers, startSymbol)) {
        case (Some((viterbi, backpointers, prevSym)), currSym) =>
          // for each possible tag, get the highest probability previous tag and its score
          val transitionScores =
            tagDict.set(currSym).mapTo(currTag => // each legal tag for the current symbol
              viterbi.collect {
                case (prevTag, viterbtiScore) if tagTransitions.getOrElse(prevTag, Set())(currTag) => // if the transition is valid
                  (prevTag, viterbtiScore * edgeScorer(prevSym, prevTag, currSym, currTag))
              })
              .toMap
              .filter(_._2.nonEmpty) // remove tags that don't transition anywhere
          val bestTransitions = transitionScores.mapVals(_.maxBy(_._2)) // get the previous tag with the highest probability (and its score)
          if (bestTransitions.nonEmpty)
            Some(
              bestTransitions.mapVals(_._2), // update viterbi for the next row
              bestTransitions.mapVals(_._1) :: backpointers, // append new backpointers
              currSym)
          else
            None
        case (None, _) => None
      }.map { case (_, backpointers, _) => backpointers }

    // Get the optimal tag sequence and map the tag indices back to their string values
    backpointers.map(bp => sequence zipSafe backtrack(bp).flatten)
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[Option[Tag], Option[Tag]]]): Vector[Option[Tag]] = {
    @tailrec def inner(backpointers: List[Map[Option[Tag], Option[Tag]]], curTag: Option[Tag], tags: List[Option[Tag]]): List[Option[Tag]] =
      backpointers match {
        case Nil => assert(curTag == None); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag :: tags)
      }
    val Pattern.Map(None -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil).toVector
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
