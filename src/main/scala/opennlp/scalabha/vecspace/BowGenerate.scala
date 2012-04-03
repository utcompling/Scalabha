package opennlp.scalabha.vecspace

import com.cloudera.scrunch.PCollection
import com.cloudera.scrunch.Pipeline
import com.cloudera.scrunch.From
import com.cloudera.scrunch.PTable
import org.apache.commons.logging.LogFactory
import org.apache.log4j.Level
import org.apache.log4j.Logger
import opennlp.scalabha.util.CollectionUtils._
import opennlp.scalabha.util.Pattern
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.LogNum

/**
 * HOW TO RUN:
 * sbt assembly
 * hadoop fs -put /scratch/01899/dhg1/nytgiga.lem nytgiga.lem
 * hadoop jar target/mln-semantics-assembly-0.0.1.jar utcompling.vectorspace.vecspace.BowGenerate nytgiga.lem
 * hadoop fs -getmerge nytgiga.lem.vc.out /scratch/01899/dhg1/nytgiga.lem.vc
 */
object BowGenerate {
  val LOG = LogFactory.getLog(BowGenerate.getClass)

  val punctuation = (w: String) => Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "...", "-", "_")(w.toUpperCase)
  val stopwords = (w: String) => punctuation(w) || Stopwords.get(w) //TODO: remove stopwords?

  val Log2 = math.log(2)

  def main(args: Array[String]) {
    Logger.getRootLogger.setLevel(Level.INFO)
    //Logger.getLogger("utcompling").setLevel(Level.DEBUG)

    val DEFAULT_NUM_FEATURES = "2000"
    val DEFAULT_MIN_WORD_COUNT = "50"
    val DEFAULT_WINDOW_SIZE = "Inf"

    var additionalArgs: List[String] = Nil
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_WINDOW_SIZE
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_MIN_WORD_COUNT
    if (args.size + additionalArgs.size < 4)
      additionalArgs ::= DEFAULT_NUM_FEATURES
    if (args.size + additionalArgs.size < 4)
      throw new RuntimeException("Expected arguments: inputFile, numFeatures, minWordCount, windowSize")

    val List(inputFile, numFeaturesString, minWordCountString, windowSizeString) = args.toList ++ additionalArgs
    val outputFile = "%s.vc.f%s.m%s.w%s".format(inputFile, numFeaturesString, minWordCountString, windowSizeString)
    val numFeatures = numFeaturesString.toInt
    val minWordCount = minWordCountString.toInt
    val windowSize = windowSizeString.toLowerCase match { case "inf" => 10000; case s => s.toInt }

    LOG.info("outputFile = " + outputFile)

    val pipeline = new Pipeline[BowGenerate]
    val inputLines =
      pipeline
        .read(From.textFile(inputFile))
        .map(_.trim) // remove trailing space
        .filter(!badLine(_)) // remove weird lines

    val tfidfs = getTfidfs(inputLines, minWordCount)
    LOG.info("computed all tf-idfs")

    val topTfidfsP = tfidfs.top(numFeatures, maximize = true)
    val topTfidfs = topTfidfsP.materialize.toMap

    if (LOG.isDebugEnabled) {
      LOG.info("ALL FEATURES")
      for ((word, (tfidf /*, (tf, df)*/ )) <- topTfidfs.toList.sorted.take(numFeatures))
        //LOG.info("\t%s\t%f\t%d\t%d".format(word, tfidf, tf, df))
        LOG.info("\t%s\t%f".format(word, tfidf))
    }

    val validWords = tfidfs.map((word, _) => word).materialize.toSet //make a set of words that exceed the minimum count
    val features = Set() ++ topTfidfs.keySet //make a serializable feature set
    val wordVectors = getBowVectors(inputLines, validWords, features, windowSize)
    LOG.info("calculated all vectors")

    pipeline.writeTextFile(stringify(wordVectors, features, (d: Double) => "%.2f".format(math.exp(d))), outputFile)

    pipeline.done
  }

  def getTfidfs(inputLines: PCollection[String], minWordCount: Int) = {
    val SentenceMarker = ""

    // Get the count of each word in the corpus
    val countsWithDummy =
      inputLines
        .flatMap(_
          .split("\\s+").toList // split into individual tokens
          .filterNot(stopwords) // remove useless tokens 
          .counts // map words to the number of times they appear in this sentence
          .map {
            // map word to its count in the sentence AND a count of 1 document 
            // that they word has appeared in. 
            case (word, count) => (word, (count, 1))
          } + (SentenceMarker -> (1, 1))) // add a dummy word to count the total number of sentences
        .groupByKey
        .combine {
          tfdfCounts =>
            val (tfCounts, dfCounts) = tfdfCounts.unzip
            (tfCounts.sum, dfCounts.sum)
        }
        .filter { case (w, (tf, df)) => tf >= minWordCount } // Keep only the non-punctuation words occurring more than MIN_COUNT times

    // Get scalar number of sentences
    val List(SentenceMarker -> (_ -> numSentences)) = countsWithDummy.filter((w, c) => w == SentenceMarker).materialize.toList
    println("numSentences = " + numSentences)

    // Get the real word counts (non-dummy)
    val counts = countsWithDummy.filter((w, c) => w != SentenceMarker)

    // Compute TF-IDF value for each word
    val tfidfs = counts.mapValues { case (tf, df) => (tf * math.log(numSentences.toDouble / df) /*, (tf, df)*/ ) } // TODO: log(tf)?

    tfidfs
  }

  def badLine(s: String) = {
    Set(
      "-LRB- STORY CAN END HERE .",
      "OPTIONAL 2ND TAKE FOLLOWS . -RRB-")(s)
  }

  def getBowVectors(inputLines: PCollection[String], validWords: Set[String], features: Set[String], windowSize: Int) = {
    val WordCountFeature = ""

    /*
     * C(f,w) and, 
     * C(w) = C(WordCountFeature, w)
     */
    val featureCountsByWordWithWordCountFeature =
      inputLines
        .flatMap { line => // take the line
          val tokens = line.split(" ").toList // split into individual tokens
          tokens.zipWithIndex.collect {
            case (token, i) if validWords(token) => // for each token that meets the cutoff
              val before = tokens.slice(i - windowSize, i) // get the tokens before it
              val after = tokens.slice(i + 1, i + 1 + windowSize) // and the tokens after it
              val featuresInWindow = ((before ++ after).filter(features)).toSet.toList // keep only the features in the window
              (WordCountFeature :: featuresInWindow).map(w => ((token, w), 1))
          }.flatten
        }
        .groupByKey
        .combine(_.sum)
        .map{ case ((token, word), count) => (token, (word, count)) }
        .groupByKey
        .map((token, counts_iter) => (token, counts_iter.toMap))

    if (LOG.isDebugEnabled) {
      LOG.debug("featureCountsByWordWithWordCountFeature")
      stringify(featureCountsByWordWithWordCountFeature, features, (i: Int) => i.toString).materialize.foreach(s => println("\t" + s))
    }

    /*
     * P(f|w) = C(f,w) / C(w)
     * represented as a log
     */
    val featureProbGivenWord =
      featureCountsByWordWithWordCountFeature
        .mapValues { featureCounts =>
          val wordCount = LogNum(featureCounts(WordCountFeature))
          featureCounts
            .filterKeys(_ != WordCountFeature) // remove dummy feature
            .mapValuesStrict(c => (LogNum(c) / wordCount).logValue)
        }

    if (LOG.isDebugEnabled) {
      LOG.debug("featureProbGivenWord")
      stringify(featureProbGivenWord, features, (p: Double) => "%.2f".format(math.exp(p))).materialize.foreach(s => println("\t" + s))
    }

    /*
     * C(f,w)
     */
    val featureCountsByWord =
      featureCountsByWordWithWordCountFeature
        .mapValues(_.filterKeys(_ != WordCountFeature)) //remove WordCountFeature

    if (LOG.isDebugEnabled) {
      LOG.debug("featureCountsByWord")
      stringify(featureCountsByWord, features, (i: Int) => i.toString).materialize.foreach(s => println("\t" + s))
    }

    /*
     * numWords = sum C(w') for all w'
     */
    val Pattern.Map(1 -> numWords) =
      featureCountsByWordWithWordCountFeature
        .map((word, featureCounts) => featureCounts(WordCountFeature)) //only WordCountFeature
        .groupBy(_ => 1)
        .combine(_.sum)
        .materialize.toMap
    LOG.info("numWords = " + numWords)

    /*
     * P(f,w) = C(f,w) / sum(C(w'))
     * represented as a log
     */
    val featureWordProbByWord =
      featureCountsByWord
        .mapValues(_
          .mapValuesStrict(c => (LogNum(c) / LogNum(numWords)).logValue))

    if (LOG.isDebugEnabled) {
      LOG.debug("featureWordProbByWord")
      stringify(featureWordProbByWord, features, (p: Double) => "%.2f".format(math.exp(p))).materialize.foreach(s => println("\t" + s))
    }

    /*
     * P(f) = sum P(f,w') for all w'
     * represented as a log
     */
    val featureProb =
      featureWordProbByWord
        .flatMap((word, featureProbs) => featureProbs)
        .groupByKey
        .combine(_.map(new LogNum(_)).sum.logValue)
        .materialize.toMap

    if (LOG.isDebugEnabled) {
      LOG.debug("featureProb")
      featureProb.map { case (f, p) => "%s\t%.2f".format(f, math.exp(p)) }.foreach(s => println("\t" + s))
    }

    /*
     * pmi(f,w) = log_2 (P(f|w) / P(f))
     * represented as a log
     */
    val pmi =
      featureProbGivenWord
        .mapValues(_.map { case (feature, prob) => (feature, (new LogNum(prob) / new LogNum(featureProb(feature))).logValue / Log2) })

    pmi
  }

  //  case class TfidfTriple(var _1: Double, var _2: Int, var _3: Int) extends java.lang.Comparable[TfidfTriple] {
  //    def this() = this(0., 0, 0)
  //    def compareTo(that: TfidfTriple): Int = this._1.compareTo(that._1)
  //  }

  def stringify[N: Numeric](wordVectors: PTable[String, Map[String, N]], features: Iterable[String], toString: N => String) = {
    wordVectors.map {
      case (word, vector) =>
        val vecString =
          features.toList.sorted.map { f =>
            vector.get(f) match {
              case Some(v) => "%s\t%s".format(f, toString(v))
              case None => "\t"
            }
          }.mkString("\t")
        "%s\t%s".format(word, vecString)
    }
    //.materialize.foreach(s => println("\t"+s))
  }

}

class BowGenerate
