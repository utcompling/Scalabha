package opennlp.scalabha.classify

import opennlp.scalabha.util.LogNum
import scala.io.Source

/**
 * Trait for document classifiers
 * @tparam L the class of labels used by this classifier
 * @tparam T the ypt of value used for the features
 */
trait Classifier[L, T] {

  /** Iterate through the set of labels for this classifier */
  def labels: Iterable[L]

  /**
   * Classify a document
   * @return a sequence of labels with their double-valued scores
   */
  def classify(document: Instance[T]): Iterable[(L, LogNum)]

  /** Return the best label provided this classifier */
  def bestLabel(document: Instance[T]) = classify(document).maxBy(_._2)
}

/**
 * Trait for classifier trainers
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
trait ClassifierTrainer[L, T]
  extends (TraversableOnce[(L, Instance[T])] => Classifier[L, T])


/**
 * Trait for classifier trainers which use distributions over the labels
 * rather than hard label annotations to train new classifiers
 * @tparam L the class of labels used by the classifiers created
 * @tparam T the class of value used for the features
 */
trait SoftClassifierTrainer[L, T] {
  def train(documents: Iterable[(Iterable[(L, LogNum)], _ <: Instance[T])]): Classifier[L, T]
}
