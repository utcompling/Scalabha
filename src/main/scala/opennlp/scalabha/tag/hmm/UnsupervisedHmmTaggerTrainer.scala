package opennlp.scalabha.tag.hmm

import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.LogNum
import scala.collection.GenIterable
import opennlp.scalabha.tag.hmm.support._
import scala.annotation.tailrec

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param initialUnsupervisedEmissionDist
 * @param estimatedTransitionCountsTransformer		factory for generating builders that count tag occurrences and compute distributions during EM
 * @param estimatedEmissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions during EM
 * @param maxIterations								maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM					stop iterating EM if change in average log probability is less than this threshold
 */
class UnsupervisedHmmTaggerTrainer[Sym, Tag](
  initialUnsupervisedEmissionDist: Option[Tag] => Option[Sym] => LogNum,
  override protected val estimatedTransitionCountsTransformer: TransitionCountsTransformer[Tag, Sym],
  override protected val estimatedEmissionCountsTransformer: EmissionCountsTransformer[Tag, Sym],
  override protected val maxIterations: Int = 50,
  override protected val minAvgLogProbChangeForEM: Double = 0.00001)
  extends AbstractEmHmmTaggerTrainer[Sym, Tag]
  with UnsupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Hidden Markov Model tagger only on unlabeled data using the
   * Expectation-Maximization (EM) algorithm.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @return						a trained tagger
   */
  override def trainUnsupervised(tagDict: TagDict[Sym, Tag], rawTrainSequences: Iterable[IndexedSeq[Sym]]): Tagger[Sym, Tag] = {
    LOG.info("Beginning unsupervised training")
    //LOG.info("Tag dict: %d symbols, %.3f avg tags/symbol".format(tagDict.size, tagDict.values.map(_.size).avg))

    // Correct tag dictionary for start/final symbols
    val tagDictWithEnds = OptionalTagDict(tagDict)

    // Create the initial distributions
    val allTags = tagDictWithEnds.allTags
    val initialTransitions = CondFreqDist(DefaultedCondFreqCounts(allTags.mapToVal((allTags + None).mapToVal(1.).toMap).toMap + (None -> allTags.mapToVal(1.).toMap)))
    val initialEmissions = initialUnsupervisedEmissionDist
    val initialHmm = new HmmTagger(initialTransitions, initialEmissions, tagDictWithEnds)

    hmmExaminationHook(initialHmm)

    // Re-estimate probability distributions using EM
    // Do not assume any known counts -- use only EM-estimated counts
    trainWithEm(rawTrainSequences, initialHmm)
  }

}

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param initialTransitionCountsTransformer	factory for generating builders that count tag occurrences and compute distributions for input to EM
 * @param initialEmissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions for input to EM
 * @param maxIterations							maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM				stop iterating EM if change in average log probability is less than this threshold
 */
class SemisupervisedHmmTaggerTrainer[Sym, Tag](
  initialTransitionCountsTransformer: TransitionCountsTransformer[Tag, Sym],
  initialEmissionCountsTransformer: EmissionCountsTransformer[Tag, Sym],
  override protected val estimatedTransitionCountsTransformer: TransitionCountsTransformer[Tag, Sym],
  override protected val estimatedEmissionCountsTransformer: EmissionCountsTransformer[Tag, Sym],
  override protected val maxIterations: Int = 50,
  override protected val minAvgLogProbChangeForEM: Double = 0.00001)
  extends SupervisedHmmTaggerTrainer[Sym, Tag](initialTransitionCountsTransformer, initialEmissionCountsTransformer)
  with AbstractEmHmmTaggerTrainer[Sym, Tag]
  with SemisupervisedTaggerTrainer[Sym, Tag] {

  /**
   * Train a Hidden Markov Model tagger from a combination of labeled data and
   * unlabeled data using the Expectation-Maximization (EM) algorithm.  Use
   * the provided tag dictionary instead of creating one from the labeled data.
   *
   * @param tagDict					a mapping from symbols to their possible tags
   * @param rawTrainSequences		unlabeled sequences to be used as unsupervised training data
   * @param taggedTrainSequences	labeled sequences to be used as supervised training data
   * @return						a trained tagger
   */
  override def trainSemisupervised(
    tagDict: TagDict[Sym, Tag],
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    taggedTrainSequences: Iterable[IndexedSeq[(Sym, Tag)]]): Tagger[Sym, Tag] = {

    // Get initial counts and probability distributions from the labeled data alone
    val (initialTransitionCounts, initialEmissionCounts) = getCountsFromTagged(taggedTrainSequences)

    // Create the initial HMM
    val initialTransitions = CondFreqDist(initialTransitionCountsTransformer(initialTransitionCounts))
    val initialEmissions = CondFreqDist(initialEmissionCountsTransformer(initialEmissionCounts))
    val initialHmm = new HmmTagger(initialTransitions, initialEmissions, OptionalTagDict(tagDict))

    hmmExaminationHook(initialHmm)

    // Re-estimate probability distributions using EM
    val hmm =
      trainWithEm(
        rawTrainSequences,
        initialHmm,
        CondFreqCounts(initialTransitionCounts).toDouble, CondFreqCounts(initialEmissionCounts).toDouble)

    hmm
  }

}

/**
 * Factory for training a Hidden Markov Model tagger from a combination of
 * labeled data and unlabeled data using the Expectation-Maximization (EM)
 * algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param estimatedTransitionCountsTransformer		factory for generating builders that count tag occurrences and compute distributions during EM
 * @param estimatedEmissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions during EM
 * @param maxIterations								maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM					stop iterating EM if change in average log probability is less than this threshold
 */
trait AbstractEmHmmTaggerTrainer[Sym, Tag] {
  protected val estimatedTransitionCountsTransformer: TransitionCountsTransformer[Tag, Sym]
  protected val estimatedEmissionCountsTransformer: EmissionCountsTransformer[Tag, Sym]
  protected val maxIterations: Int = 50
  protected val minAvgLogProbChangeForEM: Double = 0.00001

  type OSym = Option[Sym]
  type OTag = Option[Tag]

  protected val LOG = LogFactory.getLog(classOf[AbstractEmHmmTaggerTrainer[Sym, Tag]])

  def trainWithEm(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag]): HmmTagger[Sym, Tag] = {

    trainWithEm(
      rawTrainSequences,
      initialHmm,
      CondFreqCounts[OTag, OTag, Double](),
      CondFreqCounts[OTag, OSym, Double]())
  }

  def trainWithEm(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    initialTransitionCounts: CondFreqCounts[OTag, OTag, Double],
    initialEmissionCounts: CondFreqCounts[OTag, OSym, Double]): HmmTagger[Sym, Tag] = {

    reestimateLogNumDistributions(
      rawTrainSequences,
      initialHmm,
      1, Double.NegativeInfinity,
      initialTransitionCounts,
      initialEmissionCounts)
  }

  /**
   * Re-estimate probability distributions using EM.  Estimate counts for
   * each sequence in rawTrainSequences using the forward/backward procedure.
   * Calculate probability distributions from these counts.  Repeat until
   * convergence.
   */
  @tailrec
  final protected def reestimateLogNumDistributions(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    iteration: Int,
    prevAvgLogProb: Double,
    initialTransitionCounts: CondFreqCounts[OTag, OTag, Double],
    initialEmissionCounts: CondFreqCounts[OTag, OSym, Double]): HmmTagger[Sym, Tag] = {

    // E Step:  Use the forward/backward procedure to determine the 
    //          probability of various possible state sequences for 
    //          generating the training data

    val (expectedTransitionCounts, expectedEmmissionCounts, avgLogProb) =
      estimateCounts(rawTrainSequences, initialHmm, initialTransitionCounts, initialEmissionCounts)

    // M Step: Use these probability estimates to re-estimate the 
    //         probability distributions

    val transitions = CondFreqDist(estimatedTransitionCountsTransformer(expectedTransitionCounts.toMap))
    val emissions = CondFreqDist(estimatedEmissionCountsTransformer(expectedEmmissionCounts.toMap))
    val hmm = HmmTagger(transitions, emissions, initialHmm.tagDict)

    LOG.debug("\t" + iteration + ": " + avgLogProb)

    hmmExaminationHook(hmm)

    // Check each ending condition
    if (iteration >= maxIterations) {
      LOG.info("DONE: Max number of iterations reached")
      hmm
    }
    else if ((avgLogProb - prevAvgLogProb).abs < minAvgLogProbChangeForEM) { //check if converged
      LOG.info("DONE: Change in average log probability is less than " + minAvgLogProbChangeForEM)
      hmm
    }
    else if (avgLogProb < prevAvgLogProb) {
      throw new RuntimeException("DIVERGED: log probability decreased on iteration %d".format(iteration))
    }
    else if (avgLogProb == Double.NegativeInfinity) {
      throw new RuntimeException("averageLogProb == -Infinity on iteration %d".format(iteration))
    }
    else {
      // No ending condition met, re-estimate
      reestimateLogNumDistributions(
        rawTrainSequences,
        hmm,
        iteration + 1, avgLogProb,
        initialTransitionCounts,
        initialEmissionCounts)
    }
  }

  /**
   * Estimate transition and emission counts for each sequence in
   * rawTrainSequences using the forward/backward procedure.
   *
   * TODO: This method should be rewritten as a MapReduce job.
   */
  protected def estimateCounts(
    rawTrainSequences: Iterable[IndexedSeq[Sym]],
    hmm: HmmTagger[Sym, Tag],
    initialTransitionCounts: CondFreqCounts[OTag, OTag, Double],
    initialEmissionCounts: CondFreqCounts[OTag, OSym, Double]) = {

    val (expectedTransitionCounts, expectedEmissionCounts, totalSeqProb, numSequences) =
      rawTrainSequences.par
        .map {
          sequence =>
            val (estTrCounts, estEmCounts, seqProb) = estimateCountsForSequence(sequence, hmm)
            (estTrCounts, estEmCounts, seqProb.logValue, 1) // number of sentences == 1
        }
        .fold((CondFreqCounts[OTag, OTag, Double](), CondFreqCounts[OTag, OSym, Double](), 0., 0)) {
          case ((aTC, aEC, aP, aN), (bTC, bEC, bP, bN)) => (aTC ++ bTC, aEC ++ bEC, aP + bP, aN + bN) // sum up all the components
        }

    (expectedTransitionCounts ++ initialTransitionCounts, expectedEmissionCounts ++ initialEmissionCounts, totalSeqProb / numSequences)
  }

  /**
   * Estimate transition and emission counts for the given sequence using
   * the forward/backward procedure.
   */
  protected def estimateCountsForSequence(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]): (CondFreqCounts[OTag, OTag, Double], CondFreqCounts[OTag, OSym, Double], LogNum) = {

    val (forwards, forwardProb) = forwardProbabilities(sequence, hmm)
    val (backwrds, backwrdProb) = backwrdProbabilities(sequence, hmm)
    assert(forwardProb approx backwrdProb, "forward=%s, backward=%s".format(forwardProb.logValue, backwrdProb.logValue))
    val seqProb = forwardProb // P(sequence | hmm)

    // Get expected transition counts based on forward-backward probabilities
    //        Let expectedTransitionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t and state j at time t+1
    val expectedTransitionCounts = estimateTransitionCounts(sequence, hmm, forwards, backwrds, seqProb)

    // Get expected emission counts based on forward-backward probabilities
    //        Let expectedEmissionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t given the observations and the model
    val expectedEmissionCounts = estimateEmissionCounts(sequence, hmm, forwards, backwrds, seqProb)

    (expectedTransitionCounts, expectedEmissionCounts, seqProb)
  }

  /**
   * Calculate forward probabilities for the sequence based on the existing
   * transition and emission probabilities.
   *
   * Let forward(t)(j) be the probability of being in state j after seeing the
   *     first t observations (by summing over all initial paths leading to j).
   *
   *             forward(t)(j) = P(o1,o2,...,ot, q_t=j | lambda)
   */
  protected def forwardProbabilities(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]): (IndexedSeq[OTag => LogNum], LogNum) = {

    // Initialization
    //     forward(1)(j) = a(start)(j) * b(j)(o1)   j in [1,N]
    // Recursion
    //     forward(t)(j) = (1 to N).sum(i => forward(t-1)(i) * a(i)(j)) * bj(ot)    j in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = forward(final)(sf) = (1 to N).sum(i => forward(T)(i) * aif)

    val startForward: Map[OTag, LogNum] = Map(None -> LogNum.one)

    val (lastForward @ Pattern.Map(None -> forwardProb), forwards) =
      (sequence.map(Some(_)) :+ None).foldLeft((startForward, List[Map[OTag, LogNum]]())) {
        case ((prevForward, otherForwards), tok) =>
          val currForward =
            hmm.tagDict.set(tok).mapTo { currTag => // each legal tag for the current token
              val tProb =
                prevForward.sumBy {
                  case (prevTag, prevFwdScore) => prevFwdScore * hmm.transitions(prevTag)(currTag)
                }
              val eProb = hmm.emissions(currTag)(tok)
              tProb * eProb
            }.toMap
          (currForward, prevForward :: otherForwards)
      }

    ((lastForward :: forwards).reverse.toIndexedSeq, forwardProb)
  }

  /**
   * Calculate backward probabilities for the sequence based on the existing
   * transition and emission probabilities.
   *
   * Let backwrd(t)(j) be the probability of observing the final set of observations
   *     from time t+1 to T given that one is in state i at time t
   *
   *             backwrd(j) = P(o1,o2,...,ot, q_t=j | lambda)
   */
  protected def backwrdProbabilities(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag]): (IndexedSeq[OTag => LogNum], LogNum) = {

    // Initialization
    //     backwrd(T)(i) = a(i)(F)   i in [1,N]
    // Recursion
    //     backwrd(t)(i) = (1 to N).sum(j => a(i)(j) * b(j)(o_(t+1)) * backwrd(t+1)(j))    i in [1,N], t in [1,T]
    // Termination
    //     P(O | lambda) = backwrd(1)(s0) = (1 to N).sum(i => a(0)(j) * b(j)(o1) * backwrd(1)(j))

    val finalBackwrd: Map[OTag, LogNum] = Map(None -> LogNum.one)

    val (firstBackwrd @ Pattern.Map(None -> backwrdProb), backwrds, lastTok) =
      (None +: sequence.map(Some(_))).foldRight((finalBackwrd, List[Map[OTag, LogNum]](), None: OSym)) {
        case (tok, (nextBackwrd, otherBackwrds, nextTok)) =>
          val currBackwrd =
            hmm.tagDict.set(tok).mapTo { currTag =>
              nextBackwrd.sumBy {
                case (nextTag, nextBkwdScore) =>
                  hmm.transitions(currTag)(nextTag) * hmm.emissions(nextTag)(nextTok) * nextBkwdScore
              }
            }.toMap
          (currBackwrd, nextBackwrd :: otherBackwrds, tok)
      }

    ((firstBackwrd :: backwrds).toIndexedSeq, backwrdProb)
  }

  /**
   * Estimate transition counts for the sequence based on forward and
   * backward probabilities.
   *
   *    estTrans(i,j) = sum_t(fwd(t)(i) * a(i)(j) * b(o(t+1)) * bkw(t+1)(j) / seqProb)
   */
  protected def estimateTransitionCounts(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag],
    forwards: IndexedSeq[OTag => LogNum],
    backwrds: IndexedSeq[OTag => LogNum],
    seqProb: LogNum) = {

    val liftedSeq = sequence.map(Some(_))
    val validTagsByToken: IndexedSeq[Set[OTag]] = liftedSeq.map(hmm.tagDict.set)
    val noneTagset: Set[OTag] = Set(None)

    val nextTokens = liftedSeq :+ None
    val currTagSets = noneTagset +: validTagsByToken
    val nextTagSets = validTagsByToken :+ noneTagset
    val currForwards = forwards.dropRight(1)
    val nextBackwrds = backwrds.drop(1)

    val expectedTransitionCounts =
      (nextTokens zipEqual currTagSets zipEqual nextTagSets zipEqual currForwards zipEqual nextBackwrds).map {
        case ((((nextTok, currTags), nextTags), currForward), nextBackwrd) =>
          currTags.mapTo { currTag =>
            nextTags.mapTo { nextTag =>
              (currForward(currTag) * hmm.transitions(currTag)(nextTag) * hmm.emissions(nextTag)(nextTok) * nextBackwrd(nextTag) / seqProb).toDouble
            }.toMap
          }.toMap
      }

    expectedTransitionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }

  /**
   * Estimate emission counts for the sequence based on forward and
   * backward probabilities.
   *
   *   estEmiss(t)(j) = fwd(t)(j) * bkw(t)(j) / seqProb
   */
  protected def estimateEmissionCounts(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag],
    forwards: IndexedSeq[OTag => LogNum],
    backwrds: IndexedSeq[OTag => LogNum],
    seqProb: LogNum): CondFreqCounts[OTag, OSym, Double] = {

    val liftedSeq = None +: sequence.map(Some(_)) :+ None

    val expectedEmissionCounts =
      (liftedSeq zipEqual forwards zipEqual backwrds).map {
        case ((tok, forward), backwrd) =>
          hmm.tagDict.set(tok).mapTo(tag =>
            Map(tok -> (forward(tag) * backwrd(tag) / seqProb).toDouble)).toMap
      }

    expectedEmissionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }

  protected def hmmExaminationHook(hmm: HmmTagger[Sym, Tag]) {

  }
}

