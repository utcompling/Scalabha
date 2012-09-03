package opennlp.scalabha.classify.nb

import scala.collection.mutable
import opennlp.scalabha.classify.Classifier
import opennlp.scalabha.classify.ClassifierTrainer
import opennlp.scalabha.classify.Instance
import opennlp.scalabha.tag.support.AddLambdaSmoothingCondCountsTransformer
import opennlp.scalabha.tag.support.CondCountsTransformer
import opennlp.scalabha.tag.support.CountsTransformer
import opennlp.scalabha.tag.support.CondFreqDist
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.tag.support.PassthroughCondCountsTransformer
import opennlp.scalabha.tag.support.PassthroughCountsTransformer
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.LogNum

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
  val labels: Set[L]) extends Classifier[L, T] {

  private[this] lazy val uniformProbs = labels.mapToVal(LogNum.one).normalizeValues

  override def classify(document: Instance[T]): Iterable[(L, LogNum)] = {
    val features = document.allFeatures
    val unnormalizedScores = labels.mapTo { label =>
      val prior = priorProb(label)
      val featuresProb = features.map(featureProb(label)).product
      prior * featuresProb
    }

    if (unnormalizedScores.forall(_._2 == LogNum.zero))
      uniformProbs
    else
      unnormalizedScores.normalizeValues
  }
}

/**
 * A classifier trainer for an online naive Bayes classifier
 * @param lambda a smoothing parameter for add-lambda smoothing
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
case class OnlineNaiveBayesClassifierTrainer[L, T](
  labelDocCountsTransformer: CountsTransformer[L] = PassthroughCountsTransformer[L](),
  labelFeatureCountsTransformer: CondCountsTransformer[L, T] = PassthroughCondCountsTransformer[L, T]())
  extends ClassifierTrainer[L, T] {

  val labelDocCounter = mutable.Map[L, Int]().withDefaultValue(0)
  val labelFeatureCounter = mutable.Map[L, mutable.Map[T, Int]]()

  def reset {
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
      }
    }

    val labelDocDist = FreqDist(labelDocCountsTransformer(labelDocCounter))
    val labelFeatDist = CondFreqDist(labelFeatureCountsTransformer(labelFeatureCounter))
    NaiveBayesClassifier[L, T](
      labelDocDist,
      labelFeatDist,
      labelDocCounter.keys.toSet)
  }
}

object OnlineNaiveBayesClassifierTrainer {
  def apply[L, T](lambda: Double): OnlineNaiveBayesClassifierTrainer[L, T] = {
    val labelFeatureCountsTransformer =
      if (lambda == 0)
        PassthroughCondCountsTransformer[L, T]()
      else
        AddLambdaSmoothingCondCountsTransformer[L, T](lambda)
    new OnlineNaiveBayesClassifierTrainer(
      labelFeatureCountsTransformer = labelFeatureCountsTransformer)
  }
}
