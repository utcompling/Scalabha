package opennlp.scalabha.classify.nb

import opennlp.scalabha.classify._
import opennlp.scalabha.util._

/**
 * The naive Bayes classifier as a trait: defining common functionality that
 * are shared by different implementations of the NB classifier
 */
trait NaiveBayesClassifier[L,T] extends Classifier[L,T] {
  private val one = Probability(1.)

  def priorProb(label:L):Probability

  def featureProb(feature:T, label:L):Probability

  override def classify(document:Document[T]) =
    labels.map(label => {
      val prior = priorProb(label)
      val features = document.allFeatures
      val featuresProb = (one/:features.map(featureProb(_,label)))(_*_)
      (label, prior * featuresProb)
    })
}

class OnlineNaiveBayesClassifier[L,T](
    val docCount:Double,
    val labelDocCount:Map[L,Double],
    val labelFeatureCount:Map[(L,T),Double],
    val vocabulary:Set[T],
    val labels:Set[L],
    val addN:Double)
extends NaiveBayesClassifier[L,T] {

  val labelTokenSum = {
    val builder = Map.newBuilder[L,Double]
    labels.foreach({label =>
      builder += ((label, vocabulary.map(labelFeatureCount(label,_)).sum))
    })
    builder.result
  }

  override def priorProb(label:L) = Probability(labelDocCount(label) / docCount)

  override def featureProb(feature:T, label:L) = 
    Probability((labelFeatureCount(label, feature) + addN) / 
                (labelTokenSum(label) + (addN * vocabulary.size)))

  import OnlineNaiveBayesClassifier._
  def update(label:L, document:Document[T]) = 
    new OnlineNaiveBayesClassifier(docCount + 1,
                                   labelDocCount + ((label, 1)),
                                   labelFeatureCount ++
                                     docFeaturesCount(label, document),
                                   vocabulary ++ document.allFeatures.toSet,
                                   labels + label,
                                   addN)
}

object OnlineNaiveBayesClassifier {
  private def docFeaturesCount[L,T](label:L, document:Document[T]) = { 
    val builder = Map.newBuilder[(L,T),Double]
    document.allFeatures.foreach(feature => builder += (((label,feature),1)))
    builder.result
  }

  def empty[L,T](addN:Double) = 
    new OnlineNaiveBayesClassifier(0,
                                   Map.empty[L,Double],
                                   Map.empty[(L,T),Double],
                                   Set.empty[T],
                                   Set.empty[L],
                                   addN)
} 

class OnlineNaiveBayesClassifierTrainer[L,T](val addN:Double) 
extends ClassifierTrainer[L,T] {
  def train(documents:Iterable[(L,Document[T])]) = {
    var current = OnlineNaiveBayesClassifier.empty[L,T](addN)
    for ((label, document) <- documents)
      current = current.update(label, document)
    current
  }
}