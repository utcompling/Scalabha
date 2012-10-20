package opennlp.scalabha.tag.hmm

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.CollectionUtils._
import scala.io.Source
import opennlp.scalabha.tag._
import opennlp.scalabha.tag.support._
import opennlp.scalabha.tag.hmm.support._
import opennlp.scalabha.tag.TaggerEvaluator
import opennlp.scalabha.tag.ScoreResults
import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.apache.log4j.PropertyConfigurator
import org.apache.log4j.BasicConfigurator
import org.apache.commons.logging.LogFactory
import opennlp.scalabha.util.FileUtils
import opennlp.scalabha.util.CollectionUtil._
import opennlp.scalabha.util.LogNum

class HmmUtilsTests {

  @Test
  def test_addDistributionsToRawSequences() {
    val rawSequences = Vector(
      Vector("the", "dog", "walks"),
      Vector("the", "aardvark", "runs"))

    val tagDict = SimpleTagDict(Map(
      "the" -> Set('D),
      "dog" -> Set('N),
      "walks" -> Set('N, 'V),
      "runs" -> Set('V)), Set('D, 'A, 'N, 'V))

    val transitions = Map[Option[Symbol], Map[Option[Symbol], LogNum]](
      None -> Map(Some('D) -> LogNum(1.0)),
      Some('D) -> Map(Some('N) -> LogNum(0.5), Some('A) -> LogNum(0.4), Some('V) -> LogNum(0.1)),
      Some('A) -> Map(Some('N) -> LogNum(0.5), Some('V) -> LogNum(0.5)),
      Some('N) -> Map(Some('V) -> LogNum(1.0)),
      Some('V) -> Map(Some('D) -> LogNum(0.5), None -> LogNum(0.5)))
      .mapVals(_.withDefaultValue(LogNum.zero))

    val emissions = Map[Option[Symbol], Map[Option[String], LogNum]](
      None -> Map(None -> LogNum(1.0)),
      Some('D) -> Map(Some("the") -> LogNum(1.0)),
      Some('A) -> Map(Some("big") -> LogNum(1.0)),
      Some('N) -> Map(Some("dog") -> LogNum(0.6), Some("walks") -> LogNum(0.3), Some("aardvark") -> LogNum(0.1)),
      Some('V) -> Map(Some("walks") -> LogNum(0.5), Some("runs") -> LogNum(0.5)))
      .mapVals(_.withDefaultValue(LogNum.zero))

    val validTransitions = Map[Option[Symbol], Set[Option[Symbol]]](
      None -> Set(Some('D)),
      Some('D) -> Set(Some('N), Some('A)),
      Some('A) -> Set(Some('N)),
      Some('N) -> Set(Some('V)),
      Some('V) -> Set(Some('D), None))

    val newSentences = HmmUtils.addDistributionsToRawSequences(rawSequences, tagDict.opt, transitions, emissions)
    for (sent <- newSentences) {
      sent foreach println
      println
    }

    val newSentencesConstrained = HmmUtils.addDistributionsToRawSequences(rawSequences, tagDict.opt, transitions, emissions, validTransitions)
    for (sent <- newSentencesConstrained) {
      sent foreach println
      println
    }

  }

}
