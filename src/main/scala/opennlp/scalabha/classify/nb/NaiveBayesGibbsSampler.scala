package opennlp.scalabha.classify.nb

import scala.annotation.tailrec
import scala.collection.mutable.{ Buffer => MSeq }
import scala.collection.mutable.{ Map => MMap }
import scala.collection.{ Map => CMap }
import org.apache.commons.logging.LogFactory
import org.apache.log4j.Level
import org.apache.log4j.Logger
import opennlp.scalabha.classify.ClassifierEvaluator
import opennlp.scalabha.classify.DefaultInstance
import opennlp.scalabha.tag.support.BinomialFreqDist
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.tag.support.MultinomialFreqDist
import opennlp.scalabha.util.Collections.History
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.LogNum._
import opennlp.scalabha.util.LogNum
import opennlp.scalabha.util.Stats.DirichletSampler

/**
 * Gibbs Sampler for Naive Bayes.
 *
 * Allows for the training of a naive Bayes classifier from unlabeled or
 * partially-unlabeled data.
 *
 * Implementation based on:
 *   Gibbs Sampling for the Uninitiated
 *   Philip Resnik and Eric Hardisty
 *   http://www.umiacs.umd.edu/~resnik/pubs/gibbs.pdf
 */
class NaiveBayesGibbsSampler[L, T](
  labelList: Seq[L],
  burnInIterations: Int,
  trainingIterations: Int,
  trainingIterationLag: Int,
  labelPseudocounts: L => Int = Function.const(1) _,
  wordDistributionPseudocounts: L => (T => Int) = Function.const(Function.const(1) _) _,
  evaluator: Option[ClassifierEvaluator[L, T]] = None) {
  private val LOG = LogFactory.getLog("NaiveBayesGibbsSampler")

  def apply(
    unlabeledData: TraversableOnce[Seq[T]],
    labeledData: TraversableOnce[(L, Seq[T])]) = {

    // 
    // Temp
    // 
    // 

    // Labeled document word counts. Organized by label.
    val labDocs = labeledData.toIterator.groupByKey.mapValuesStrict(_.map(_.counts)) +++ labelList.mapToVal(List()).toMap

    //
    // Stable
    //

    // Unlabeled document word counts.
    val docs = unlabeledData.map(_.counts).toIndexedSeq

    // A vector of all known words
    val vocabulary = {
      val labWords = labDocs.iterator.flatMap(_._2).flatten.map(_._1)
      val unlWords = docs.iterator.flatten.map(_._1)
      (labWords.toSet ++ unlWords).toIndexedSeq
    }

    // Word pseudocounts, computed on demand for space concerns
    def labelWordPseudocounts(l: L) = vocabulary.iterator.mapTo(wordDistributionPseudocounts(l))

    //
    // Initial
    //

    // Randomly choose an initial labeling based only label distribution of 
    // labeled data and the label pseudocounts 
    val initialLabels = { //  L^(t)
      def priorLabelCounts(l: L) = labDocs(l).size + labelPseudocounts(l)
      val sampler = DirichletSampler(labelList, priorLabelCounts)
      MSeq.fill(docs.size) {
        val prior = sampler.sample //  pi
        FreqDist(prior).sample
      }
    }

    // Randomly generate an initial word distribution for each label based on
    // the labeled data and the word pseudocounts
    val initialWordDist = makeWordDist(vocabulary, labDocs(_).sumByKey.withDefaultValue(0))

    //
    // Volatile
    //

    // Load up the volatile store of label counts with all of the initial data
    // including labeled, (noisily-labeled) unlabeled data, and pseudocounts
    val labelCounts = {
      val unlCounts = initialLabels.counts
      val labCounts = labDocs.mapValuesStrict(_.size)
      labelList.mapTo { l =>
        val labCount = labCounts.getOrElse(l, 0)
        val unlCount = unlCounts.getOrElse(l, 0)
        val pseudo = labelPseudocounts(l)
        labCount + unlCount + pseudo
      }.toMMap
    }

    // Load up the volatile store of word counts with all of the initial data
    // including labeled and (noisily-labeled) unlabeled data BUT NOT 
    // pseudocounts.  We leave pseudocounts out to keep the vectors sparse.
    val wordCounts = {
      val unlCountsByLabel = (initialLabels zipEqual docs).groupByKey.mapValuesStrict(_.sumByKey)
      labelList.mapTo { l =>
        val labCounts = labDocs(l).sumByKey
        val unlCounts = unlCountsByLabel.getOrElse(l, Map())
        (labCounts +++ unlCounts).toMMap.withDefaultValue(0)
      }.toMap
    }

    //
    //
    //

    val numDocs = labelCounts.values.sum
    val labels = History(trainingIterations, trainingIterationLag) ::= initialLabels
    val wordDists = History(trainingIterations, trainingIterationLag) ::= initialWordDist

    @tailrec def inner(iterationsLeft: Int) {
      if (iterationsLeft > 0) {
        LOG.info("Iteration %s  (%s/%s burn-in) (%s/%s training)".format(
          burnInIterations + trainingIterations - iterationsLeft + 1,
          (burnInIterations + trainingIterations - iterationsLeft + 1) min burnInIterations, burnInIterations,
          (trainingIterations - iterationsLeft + 1) max 0, trainingIterations))

        for (((curLabel, doc), docId) <- labels.head zipEqual docs zipWithIndex) {
          // Remove current document count information
          labelCounts(curLabel) -= 1
          val countsForCurLabel = wordCounts(curLabel)
          doc.foreach { case (word, count) => countsForCurLabel(word) -= count }

          // Sample Label
          val b = numDocs - 1. //  total count of documents
          val estimates = labelList.par.mapTo { x: L => // for each potential label
            val a = labelCounts(x) - 1 //  count of documents with the potential label
            val wordDist = wordDists.head(x)
            val c = doc.map { case (word, count) => wordDist(word) ** count }.product // probability this document has the potential label
            (a / b) * c
          }.seq
          val newLabelDist = MultinomialFreqDist(estimates.normalizeValues.toMap)
          val newLabel = newLabelDist.sample //  sample a new label for this document based on the likelihood of each potential
          labels.head(docId) = newLabel

          // Put back counts for current document, but into new label's buckets
          labelCounts(newLabel) += 1
          val countsForNewLabel = wordCounts(newLabel)
          doc.foreach { case (word, count) => countsForNewLabel(word) += count }

          if (docId > 0 && docId % 10000 == 0) LOG.info("  finished document " + docId)
        }

        wordDists ::= makeWordDist(vocabulary, wordCounts) // wordCounts contains unlabeled and labeled counts 
        labels ::= labels.head

        evaluator.foreach(_(makeClassifier(labDocs, labels, wordDists)))

        inner(iterationsLeft - 1)
      }
    }

    inner(burnInIterations + trainingIterations)
    LOG.info("Final label distribution = " + labelCounts)
    makeClassifier(labDocs, labels, wordDists)
  }

  private def makeWordDist(vocabulary: Seq[T], wordCounts: L => T => Int) = {
    labelList.mapTo { l =>
      val pseudo = wordDistributionPseudocounts(l)
      val wordCountsForLabel = wordCounts(l)
      def counts(t: T) = wordCountsForLabel(t) + pseudo(t)
      DirichletSampler(vocabulary, counts).sample //  theta_l
    }.toMap
  }

  private def makeClassifier(labDocs: Map[L, List[Map[T, Int]]], labels: History[MSeq[L]], wordDists: History[Map[L, Map[T, LogNum]]]) = {
    val labeledLabelCounts = labDocs.mapValuesStrict(_.size)
    val avgLabelCounts = labeledLabelCounts +++ labels.iterator.toList.transpose.map(_.counts.maxBy(_._2)._1).counts
    val labelDocDist = FreqDist(avgLabelCounts)
    val avgWordDistributions = wordDists.iterator.flatten.groupByKey.mapValuesStrict(_.sumByKey)
    val labelFeatDist = CondFreqDist(avgWordDistributions)
    NaiveBayesClassifier(labelDocDist, labelFeatDist, labelList.toSet)
  }

  class EnrichedWithToMMap[K, V](self: Iterable[(K, V)]) { def toMMap = MMap() ++ self.toSeq }
  implicit def addToMMap[K, V](self: Iterable[(K, V)]): EnrichedWithToMMap[K, V] = new EnrichedWithToMMap(self)
}
