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

/**
 * An application that reads in a standard format set of CSVs representing
 * instances, trains a Naive Bayes model, and then outputs the predictions
 * on a second set of instances (ignoring the labels).
 */
object Classify {

  import org.clapper.argot._
  import java.io.File

  val parser =
    new ArgotParser("classify with Naive Bayes",
      preUsage = Some("Scalabha v0.2.x"))

  val trainSourceOption =
    parser.option[Source](
      List("t", "train"), "<file>", "labeled file for training model") {
        (filename, opt) =>
          val file = new File(filename)
          if (!file.exists)
            parser.usage("Training file \"" + filename + "\" does not exist.")
          Source.fromFile(file)
      }

  val predictSourceOption =
    parser.option[Source](
      List("p", "predict"), "<file>", "(labeled) file to make predictions on") {
        (filename, opt) =>
          val file = new File(filename)
          if (!file.exists)
            parser.usage("Input file \"" + filename + "\" does not exist.")
          Source.fromFile(file)
      }

  val lambdaOption =
    parser.option[Double](List("l", "lambda"), "<double>",
      "smoothing amount > 0.0 (default: 1.0)") {
        (lambdaString, opt) =>
          val lambda = lambdaString.toDouble
          if (lambda < 0.0)
            parser.usage("Lambda value should be greater than zero.")
          lambda
      }

  def main(args: Array[String]) {
    import opennlp.scalabha.util.CollectionUtils._

    try { parser.parse(args) }
    catch { case e: ArgotUsageException => println(e.message); System.exit(0) }

    // Check and instantiate the options.
    val trainSource = trainSourceOption.value match {
      case Some(tsource) => tsource
      case None => parser.usage("No training file provided.")
    }

    val predictSource = predictSourceOption.value match {
      case Some(psource) => psource
      case None => parser.usage("No input file provided.")
    }

    val lambda = lambdaOption.value.getOrElse(1.0)

    // Train the classifier
    val trainer = nb.OnlineNaiveBayesClassifierTrainer[String, String](lambda)
    val classifier =
      trainer(new CsvLabeledInstanceSource(trainSource).getLabeledInstances)

    // Run classifier on each instance to get the predictions
    val predictions =
      new CsvLabeledInstanceSource(predictSource)
        .getLabeledInstances
        .map(_._2)
        .map(instance => classifier.classify(instance))

    // Output the predictions in reverse sorted order, space separated
    predictions.foreach { prediction =>
      println(
        prediction
          .toList
          .sortBy(_._2)
          .reverse
          .map { case (l, p) => l + " " + p.toDouble }
          .mkString(" "))
    }
  }

}
