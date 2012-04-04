package opennlp.scalabha.classify

object ClassifyScorer {

  import org.clapper.argot._
  import org.clapper.argot.ArgotConverters._
  import scala.io.Source
  import java.io.File

  val parser =
    new ArgotParser("score classifier output",
      preUsage = Some("Scalabha v0.2.x"))

  val goldSourceOption =
    parser.option[Source](
      List("g", "gold"), "<file>", "file w/ gold labels") {
        (filename, opt) =>
          val file = new File(filename)
          if (!file.exists)
            parser.usage("Gold file \"" + filename + "\" does not exist.")
          Source.fromFile(file)
      }

  val predictedSourceOption =
    parser.option[Source](
      List("p", "predict"), "<file>", "file w/ predicted labels") {
        (filename, opt) =>
          val file = new File(filename)
          if (!file.exists)
            parser.usage("Predicted file \"" + filename + "\" does not exist.")
          Source.fromFile(file)
      }

  def main(args: Array[String]) {
    try { parser.parse(args) }
    catch { case e: ArgotUsageException => println(e.message); System.exit(0) }

    // Get the gold labels, but fail if no file was provided.
    val goldSource = goldSourceOption.value match {
      case Some(gsource) => gsource
      case None => parser.usage("\n\nNo gold file provided!")
    }

    // If the predicted input is given via the option, use that source;
    // otherwise, use stdin.
    val predictedSource = predictedSourceOption.value match {
      case Some(psource) => psource
      case None => Source.fromInputStream(System.in)
    }

    val goldLabels = goldSource.getLines.map(_.split(",").last).toList
    val predictedLabels = predictedSource.getLines.map(_.split(" ").head).toList

    if (goldLabels.length != predictedLabels.length) {
      println("ERROR: Different number of gold and predicted labels!")
      println("\tNum gold labels:" + goldLabels.length)
      println("\tNum predicted labels:" + predictedLabels.length)
      println("Exiting.")
      System.exit(0)
    }

    // Zip the gold and predicted lists together, filter for equality,
    // and get the length to get the number that matched.
    val numCorrect = 
      goldLabels.zip(predictedLabels).filter{case(x,y) => x == y}.length

    val accuracy = numCorrect.toDouble / goldLabels.length * 100
    println("Accuracy: " + "%1.2f".format(accuracy))
  }

}