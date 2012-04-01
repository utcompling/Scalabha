package opennlp.scalabha.classify.nb

import scala.collection.mutable
import opennlp.scalabha.classify._
import opennlp.scalabha.util._
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.tag.support._

/**
 * The naive Bayes classifier as a trait: defining common functionality that
 * are shared by different implementations of the NB classifier
 * @param priorProb a probability distribution over the labels
 * @param featureProb a function from labels to probability distributions over
 *   the features
 * @param labels the set of labels
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
case class NaiveBayesClassifier[L, T](
  val priorProb: L => LogNum,
  val featureProb: L => T => LogNum,
  val labels: Set[L])
  extends Classifier[L, T] {

  override def classify(document: Instance[T]) =
    labels.map(label => {
      val prior = priorProb(label)
      val features = document.allFeatures
      val featuresProb = features.map(featureProb(label)(_)).product
      (label, prior * featuresProb)
    })
}

/**
 * A classifier trainer for an online naive Bayes classifier
 * @param lambda a smoothing parameter for add-lambda smoothing
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
case class OnlineNaiveBayesClassifierTrainer[L, T](val lambda: Double)
  extends ClassifierTrainer[L, T] {

  val labelDocCountsTransformer = PassthroughCountsTransformer[L]()
  val labelFeatureCountsTransformer =
    if (lambda == 0)
      PassthroughCondCountsTransformer[L, T]()
    else
      AddLambdaSmoothingCondCountsTransformer[L, T](lambda)

  val labels = mutable.Set.empty[L]
  val labelDocCounter = mutable.Map[L, Int]().withDefaultValue(0)
  val labelFeatureCounter = mutable.Map[L, mutable.Map[T, Int]]()

  def reset {
    labels.clear
    labelDocCounter.clear
    labelFeatureCounter.clear
  }

  def apply(labeledDocuments: TraversableOnce[(L, Instance[T])]) = {

    labeledDocuments.foreach {
      case (label, document) => {
        labelDocCounter(label) += 1
        document.allFeatures.counts.foreach {
          case (feature, count) =>
            labelFeatureCounter.getOrElseUpdate(
              label,
              mutable.Map[T, Int]().withDefaultValue(0))(feature) += count
        }
        labels += label
      }
    }

    val labelDocDist = FreqDist(labelDocCountsTransformer(labelDocCounter))
    val labelFeatDist =
      CondFreqDist(labelFeatureCountsTransformer(labelFeatureCounter))
    NaiveBayesClassifier(labelDocDist, labelFeatDist, labels.toSet)
  }
}
