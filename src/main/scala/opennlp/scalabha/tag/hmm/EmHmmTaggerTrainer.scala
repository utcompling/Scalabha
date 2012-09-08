package opennlp.scalabha.tag.hmm

import scala.annotation.tailrec
import org.apache.commons.logging.LogFactory
import opennlp.scalabha.tag.OptionalTagDict
import opennlp.scalabha.tag.TagDict
import opennlp.scalabha.tag.TagDict._
import opennlp.scalabha.tag.Tagger
import opennlp.scalabha.tag.TypesupervisedTaggerTrainer
import opennlp.scalabha.tag.support.CondFreqCounts
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.support.DefaultedCondFreqCounts
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }

/**
 * Factory for training a Hidden Markov Model (HMM) tagger using the
 * Expectation-Maximization (EM) algorithm.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitionCountsTransformer		transforms counts from sentences tagged by EM-HMM
 * @param emissionCountsTransformer			transforms counts from sentences tagged by EM-HMM
 * @param hmmTaggerFactory					factory for constructing an HMM from transition and emission distributions
 * @param maxIterations						maximum number of iterations to be run during EM
 * @param minAvgLogProbChangeForEM			stop iterating EM if change in average log probability is less than this threshold
 */
class EmHmmTaggerTrainer[Sym, Tag](
  transitionCountsTransformer: TransitionCountsTransformer[Tag],
  emissionCountsTransformer: EmissionCountsTransformer[Tag, Sym],
  hmmTaggerFactory: HmmTaggerFactory[Sym, Tag],
  maxIterations: Int = 50,
  minAvgLogProbChangeForEM: Double = 0.00001)
  extends TypesupervisedHmmTaggerTrainer[Sym, Tag](
    new SupervisedHmmTaggerTrainer[Sym, Tag](transitionCountsTransformer, emissionCountsTransformer, hmmTaggerFactory)) {

  type OSym = Option[Sym]
  type OTag = Option[Tag]

  protected val LOG = LogFactory.getLog(classOf[EmHmmTaggerTrainer[Sym, Tag]])

  /**
   * @inheritdoc
   *
   * Runs the EM algorithm and then performs auto-supervised training on a
   * version of the corpus tagged by EM-output tagger.
   */
  override def trainFromInitialHmm(
    rawSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    tagDict: TagDict[Sym, Tag],
    priorTransitionCounts: Map[Option[Tag], Map[Option[Tag], Double]],
    priorEmissionCounts: Map[Option[Tag], Map[Option[Sym], Double]]): HmmTagger[Sym, Tag] = {

    // Train an HMM using EM
    val emHmm =
      estimateHmmWithEm(
        rawSequences,
        initialHmm,
        tagDict.opt,
        priorTransitionCounts,
        priorEmissionCounts)

    // Use the EM-trained HMM to tag the raw corpus
    val autoTagged = emHmm.tag(rawSequences.toSeq)
    val (transitionCounts, emissionCounts) = HmmUtils.getCountsFromTagged(autoTagged)

    val transitionCountsWithPriors = CondFreqCounts(transitionCounts).toDoubles ++ priorTransitionCounts toMap
    val emissionCountsWithPriors = CondFreqCounts(emissionCounts).toDoubles ++ priorEmissionCounts toMap

    // Perform supervised training (presumably with smoothing counts transformers) 
    // on the auto-tagged corpus to smooth out the EM output 
    supervisedHmmTaggerTrainer.makeTagger(transitionCountsWithPriors, emissionCountsWithPriors)
  }

  protected def estimateHmmWithEm(
    rawSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag],
    priorTransitionCounts: Map[Option[Tag], Map[Option[Tag], Double]],
    priorEmissionCounts: Map[Option[Tag], Map[Option[Sym], Double]]): HmmTagger[Sym, Tag] = {

    estimateHmmWithEm(
      rawSequences,
      initialHmm,
      tagDict,
      priorTransitionCounts,
      priorEmissionCounts,
      maxIterations, Double.NegativeInfinity)
  }

  /**
   * Re-estimate probability distributions using EM.  Estimate counts for
   * each sequence in rawSequences using the forward/backward procedure.
   * Calculate probability distributions from these counts.  Repeat until
   * convergence.
   */
  @tailrec
  final protected def estimateHmmWithEm(
    rawSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag],
    priorTransitionCounts: Map[Option[Tag], Map[Option[Tag], Double]],
    priorEmissionCounts: Map[Option[Tag], Map[Option[Sym], Double]],
    remainingIterations: Int,
    prevAvgLogProb: Double): HmmTagger[Sym, Tag] = {

    val (hmm, avgLogProb) = reestimateHmm(rawSequences, initialHmm, tagDict, priorTransitionCounts, priorEmissionCounts)

    LOG.debug("\t" + (maxIterations - remainingIterations + 1) + ": " + avgLogProb)

    hmmExaminationHook(hmm)

    // Check each ending condition
    if (remainingIterations <= 1) {
      LOG.info("DONE: Max number of iterations reached")
      hmm
    }
    else if ((avgLogProb - prevAvgLogProb).abs < minAvgLogProbChangeForEM) { //check if converged
      LOG.info("DONE: Change in average log probability is less than " + minAvgLogProbChangeForEM)
      hmm
    }
    else if (avgLogProb < prevAvgLogProb) {
      throw new RuntimeException("DIVERGED: log probability decreased on iteration %d".format(maxIterations - remainingIterations + 1))
    }
    else if (avgLogProb == Double.NegativeInfinity) {
      throw new RuntimeException("averageLogProb == -Infinity on iteration %d".format(maxIterations - remainingIterations + 1))
    }
    else {
      // No ending condition met, re-estimate
      estimateHmmWithEm(
        rawSequences,
        hmm,
        tagDict,
        priorTransitionCounts,
        priorEmissionCounts,
        remainingIterations - 1, avgLogProb)
    }
  }

  protected def reestimateHmm(
    rawSequences: Iterable[IndexedSeq[Sym]],
    initialHmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag],
    priorTransitionCounts: Map[Option[Tag], Map[Option[Tag], Double]],
    priorEmissionCounts: Map[Option[Tag], Map[Option[Sym], Double]]) = {

    // E Step:  Use the forward/backward procedure to determine the 
    //          probability of various possible state sequences for 
    //          generating the training data

    val (expectedTransitionCounts, expectedEmmissionCounts, avgLogProb) =
      estimateCounts(rawSequences, initialHmm, tagDict)

    val transitionCountsWithPriors = CondFreqCounts(expectedTransitionCounts) ++ priorTransitionCounts toMap
    val emissionCountsWithPriors = CondFreqCounts(expectedEmmissionCounts) ++ priorEmissionCounts toMap

    // M Step: Use these probability estimates to re-estimate the 
    //         probability distributions

    val transitions = CondFreqDist(transitionCountsWithPriors)
    val emissions = CondFreqDist(emissionCountsWithPriors)
    (hmmTaggerFactory(transitions, emissions), avgLogProb)
  }

  /**
   * Estimate transition and emission counts for each sequence in
   * rawSequences using the forward/backward procedure.
   *
   * TODO: This method should be rewritten as a MapReduce job.
   */
  protected def estimateCounts(
    rawSequences: Iterable[IndexedSeq[Sym]],
    hmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag]) = {

    val (expectedTransitionCounts, expectedEmissionCounts, totalSeqProb, numSequences) =
      rawSequences.par
        .map {
          sequence =>
            val (estTrCounts, estEmCounts, seqProb) = estimateCountsForSequence(sequence, hmm, tagDict)
            (estTrCounts, estEmCounts, seqProb.logValue, 1) // number of sentences == 1
        }
        .fold((CondFreqCounts[OTag, OTag, Double](), CondFreqCounts[OTag, OSym, Double](), 0., 0)) {
          case ((aTC, aEC, aP, aN), (bTC, bEC, bP, bN)) => (aTC ++ bTC, aEC ++ bEC, aP + bP, aN + bN) // sum up all the components
        }

    (expectedTransitionCounts.toMap, expectedEmissionCounts.toMap, totalSeqProb / numSequences)
  }

  /**
   * Estimate transition and emission counts for the given sequence using
   * the forward/backward procedure.
   */
  protected def estimateCountsForSequence(
    sequence: IndexedSeq[Sym],
    hmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag]): (CondFreqCounts[OTag, OTag, Double], CondFreqCounts[OTag, OSym, Double], LogNum) = {

    val (forwards, forwardProb) = forwardProbabilities(sequence, hmm, tagDict)
    val (backwrds, backwrdProb) = backwrdProbabilities(sequence, hmm, tagDict)
    assert(forwardProb approx backwrdProb, "forward=%s, backward=%s".format(forwardProb.logValue, backwrdProb.logValue))
    val seqProb = forwardProb // P(sequence | hmm)

    // Get expected transition counts based on forward-backward probabilities
    //        Let expectedTransitionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t and state j at time t+1
    val expectedTransitionCounts = estimateTransitionCounts(sequence, hmm, tagDict, forwards, backwrds, seqProb)

    // Get expected emission counts based on forward-backward probabilities
    //        Let expectedEmissionCounts(t)(i)(j) be the probability of being in 
    //            state i at time t given the observations and the model
    val expectedEmissionCounts = estimateEmissionCounts(sequence, hmm, tagDict, forwards, backwrds, seqProb)

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
    hmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag]): (IndexedSeq[OTag => LogNum], LogNum) = {

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
            tagDict.set(tok).mapTo { currTag => // each legal tag for the current token
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
    hmm: HmmTagger[Sym, Tag],
    tagDict: OptionalTagDict[Sym, Tag]): (IndexedSeq[OTag => LogNum], LogNum) = {

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
            tagDict.set(tok).mapTo { currTag =>
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
    tagDict: OptionalTagDict[Sym, Tag],
    forwards: IndexedSeq[OTag => LogNum],
    backwrds: IndexedSeq[OTag => LogNum],
    seqProb: LogNum) = {

    val liftedSeq = sequence.map(Some(_))
    val validTagsByToken: IndexedSeq[Set[OTag]] = liftedSeq.map(tagDict.set)
    val noneTagset: Set[OTag] = Set(None)

    val nextTokens = liftedSeq :+ None
    val currTagSets = noneTagset +: validTagsByToken
    val nextTagSets = validTagsByToken :+ noneTagset
    val currForwards = forwards.dropRight(1)
    val nextBackwrds = backwrds.drop(1)

    val expectedTransitionCounts =
      (nextTokens zipSafe currTagSets zipSafe nextTagSets zipSafe currForwards zipSafe nextBackwrds).map {
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
    tagDict: OptionalTagDict[Sym, Tag],
    forwards: IndexedSeq[OTag => LogNum],
    backwrds: IndexedSeq[OTag => LogNum],
    seqProb: LogNum): CondFreqCounts[OTag, OSym, Double] = {

    val liftedSeq = None +: sequence.map(Some(_)) :+ None

    val expectedEmissionCounts =
      (liftedSeq zipSafe forwards zipSafe backwrds).map {
        case ((tok, forward), backwrd) =>
          tagDict.set(tok).mapTo(tag =>
            Map(tok -> (forward(tag) * backwrd(tag) / seqProb).toDouble)).toMap
      }

    expectedEmissionCounts.map(CondFreqCounts(_)).reduce(_ ++ _)
  }

  protected def hmmExaminationHook(hmm: HmmTagger[Sym, Tag]) {}
}
