package opennlp.scalabha.classify.nb

import scala.collection.mutable
import opennlp.scalabha.classify._
import opennlp.scalabha.util._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * The naive Bayes classifier as a trait: defining common functionality that
 * are shared by different implementations of the NB classifier
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
trait NaiveBayesClassifier[L,T] extends Classifier[L,T] {

  def priorProb(label:L):Probability

  def featureProb(feature:T, label:L):Probability

  override def classify(document:Document[T]) =
    labels.map(label => {
      val prior = priorProb(label)
      val features = document.allFeatures
      val featuresProb = features.map(featureProb(_,label)).product
      (label, prior * featuresProb)
    })
}

/**
 * An immmutable naive Bayes classifier with pre-computed prior and feature
 * probabilities
 * @param priorPr a probability distribution over the labels
 * @param featurePr a function from labels to probability distributions over
 *   the features
 * @param labels the set of labels
 * @tparam L the class of labels
 * @tparam T the class of features
 */
case class CompiledNaiveBayesClassifier[L,T](
    priorPr:L => Probability,
    featurePr:L => T => Probability,
    val labels:Set[L])
extends NaiveBayesClassifier[L,T] {

  override def priorProb(label:L) = priorPr(label)

  override def featureProb(feature:T, label:L) = featurePr(label)(feature)
}

/**
 * Implementation of the naive Bayes classifier which can be updated
 * online with new training examples; this is not a mutable structure
 * per se, but can create an updated classifier given a new training
 * example
 * @param labelDocCountsTransformer CountsTransformer for converting the counts
 *   of documents associated with each label into a smoothed frequency
 *   distribution
 * @param labelFeatureCountsTransformer CondCountsTransformer for converting
 *   the counts of features wrt the labels into a smoothed frequency
 *   distribution
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
class OnlineNaiveBayesClassifier[L,T](
    val labelDocCountsTransformer:CountsTransformer[L],
    val labelFeatureCountsTransformer:CondCountsTransformer[L,T],
    val labels:mutable.Set[L])
extends NaiveBayesClassifier[L,T] {

  val labelDocCounter = mutable.Map[L, Int]().withDefaultValue(0)
  val labelFeatureCounter = mutable.Map[L, mutable.Map[T, Int]]()

  def compiled = {
    val labelDocDist = FreqDist(labelDocCountsTransformer(labelDocCounter))
    val labelFeatDist =
      CondFreqDist(labelFeatureCountsTransformer(labelFeatureCounter))
    CompiledNaiveBayesClassifier(labelDocDist, labelFeatDist, labels.toSet)
  }

  override def priorProb(label:L) = FreqDist(labelDocCountsTransformer(labelDocCounter))(label)

  override def featureProb(feature:T, label:L) =
    CondFreqDist(labelFeatureCountsTransformer(labelFeatureCounter))(label)(feature)

  def update(label:L, document:Document[T]) = {
    labelDocCounter(label) += 1
    document.allFeatures.counts.foreach{
      case (feature, count) => 
        labelFeatureCounter.getOrElseUpdate(label, mutable.Map[T, Int]().withDefaultValue(0))(feature) += count
    }
    labels += label
  }
}

/**
 * Utilities for OnlineNaiveBayesClassifier
 */
object OnlineNaiveBayesClassifier {

  /**
   * Creates a new online naive Bayes classifier with 0s for counts for
   * features and documents
   * @param lambda smoothing parameter for add-lambda smoothing
   */
  def empty[L,T](lambda:Double) = {
    val docCounter = PassthroughCountsTransformer[L]()
    val featureCounter =
      if (lambda == 0)
        PassthroughCondCountsTransformer[L,T]()
      else
        AddLambdaSmoothingCondCountsTransformer[L,T](lambda)
    val labels = mutable.Set.empty[L]
    new OnlineNaiveBayesClassifier(docCounter, featureCounter, labels)
  }
}

/**
 * A classifier trainer for an online naive Bayes classifier
 * @param lambda a smoothing parameter for add-lambda smoothing
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
case class OnlineNaiveBayesClassifierTrainer[L,T](val lambda:Double) 
extends ClassifierTrainer[L,T] {
  def train(labeledDocuments:TraversableOnce[(L,Document[T])]) = {
    val current = OnlineNaiveBayesClassifier.empty[L,T](lambda)
    labeledDocuments.foreach {
      case (label, document) => current.update(label, document)
    }
    current
  }
}
