package opennlp.scalabha.classify.nb

import org.apache.log4j.Level
import org.apache.log4j.Logger

import opennlp.scalabha.tag.support.BinomialFreqDist
import opennlp.scalabha.tag.support.DiscreteDistribution
import opennlp.scalabha.tag.support.FreqDist
import opennlp.scalabha.util.LogNum

/**
 * Generate random documents from a Naive Bayes model.
 */
class NaiveBayesGenerator[L, F](
  priorProb: DiscreteDistribution[L],
  featureProb: L => DiscreteDistribution[F]) {

  def generate(numFeatures: Int): (L, Seq[F]) = {
    generate(priorProb.sample, numFeatures)
  }

  def generate(label: L, numFeatures: Int): (L, Seq[F]) = {
    val features = Vector.fill(numFeatures)(featureProb(label).sample)
    (label, features)
  }
}

object NaiveBayesGenerator {

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)

    val g = new NaiveBayesGenerator(
      priorProb = BinomialFreqDist("N", "S", LogNum(0.8)),
      featureProb = Map(
        "N" -> FreqDist(Map("A" -> 38, "B" -> 30, "C" -> 20, "D" -> 10, "E" -> 1, "F" -> 1)),
        "S" -> FreqDist(Map("A" -> 1, "B" -> 1, "C" -> 10, "D" -> 20, "E" -> 30, "F" -> 38))))

    for (i <- 1 to 10)
      println(g.generate(4))
  }

}
