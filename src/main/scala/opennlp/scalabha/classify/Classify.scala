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
  import org.clapper.argot.ArgotConverters._
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

  val outputOption =
    parser.option[String](
      List("o", "out"), "<file>", "file to output predictions to")

  val methodOption =
    parser.option[String](
      List("m", "method"), "<method>", "classification method (naive-bayes, perceptron, pa-perceptron), default naive-bayes") {
        (method, opt) =>
          method match {
            case "nb" => "naive-bayes"
            case "naive-bayes" => "naive-bayes"
            case "perceptron" => "perceptron"
            case "pa-perceptron" => "pa-perceptron"
            case _ =>
              parser.usage("Method should be 'naive-bayes', 'perceptron' or 'pa-perceptron'.")
          }
      }

  val lambdaOption =
    parser.option[Double](List("l", "lambda"), "<double>",
      "for Naive Bayes: smoothing amount >= 0.0 (default: 0.0)") {
        (lambdaString, opt) =>
          val lambda = lambdaString.toDouble
          if (lambda < 0.0)
            parser.usage("Lambda value should be greater than or equal to zero.")
          lambda
      }

  val variantOption =
    parser.option[Int](List("v", "variant"), "<int>",
      "for perceptron: passive-aggressive variant (0, 1, 2, default: 0)") {
        (variantString, opt) =>
          val variant = variantString.toInt
          if (variant < 0 || variant > 2)
            parser.usage("Variant should be 0, 1 or 2.")
          variant
      }

  val aggressivenessOption =
    parser.option[Double](List("a", "aggressiveness"), "<double>",
      "for perceptron: aggressiveness factor > 0.0 (default: 20.0)") {
        (aggressivenessString, opt) =>
          val aggressiveness = aggressivenessString.toDouble
          if (aggressiveness <= 0.0)
            parser.usage("Aggressiveness value should be strictly greater than zero.")
          aggressiveness
      }

  def main(args: Array[String]) {
    import opennlp.scalabha.util.CollectionUtils._
    import java.io.BufferedWriter
    import java.io.FileWriter
    import java.io.OutputStreamWriter

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

    // If the output file is given via the option, create and write to that 
    // file; otherwise, use stdout.
    val output: BufferedWriter = outputOption.value match {
      case Some(outfile) => new BufferedWriter(new FileWriter(outfile))
      case None => new BufferedWriter(new OutputStreamWriter(System.out))
    }

    val method = methodOption.value match {
      case Some(meth) => meth
      case None => "naive-bayes"
    }

    val lambda = lambdaOption.value.getOrElse(0.0)
    val aggressiveness = aggressivenessOption.value.getOrElse(20.0)
    val variant = variantOption.value.getOrElse(0)

    if (method == "naive-bayes") {
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
        output.write(
          prediction
            .toList
            .sortBy(_._2)
            .reverse
            .map { case (l, p) => l + " " + p.toDouble }
            .mkString(" ")
            + "\n")
      }
    } else {
      // Train the classifier
      val factory = new perceptron.SparseNominalInstanceFactory
      val training_instances =
        factory.get_csv_labeled_instances(trainSource).toList
      val test_instances =
        factory.get_csv_labeled_instances(predictSource).toList
      val numlabs = factory.number_of_labels
      if (numlabs < 2) {
        println("Found %d different labels, when at least 2 are needed." format
          numlabs)
        System.exit(0)
      }

      // Train a classifer
      val trainer =
        if (numlabs > 2)
          new perceptron.PassiveAggressiveMultiClassPerceptronTrainer(variant,
            aggressiveness)
        else if (method == "pa-perceptron")
          new perceptron.PassiveAggressiveBinaryPerceptronTrainer(variant,
            aggressiveness)
        else
          new perceptron.BasicBinaryPerceptronTrainer(aggressiveness)
      val classifier = trainer(training_instances, numlabs)

      // Run classifier on each instance to get the predictions, and output
      // them in reverse sorted order, space separated
      for ((inst, label) <- test_instances) {
        // Mapping from labels to scores
        val scores = ((0 until numlabs).map(factory.index_to_label(_)) zip
          classifier.score(inst))
        // Output in reverse sorted order
        val outstr = scores
            .sortBy(_._2)
            .reverse
            .map { case (l, p) => l + " " + p }
            .mkString(" ")
        output.write(outstr + "\n")
      }
    }
    output.flush
    output.close
  }

}
