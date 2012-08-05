package opennlp.scalabha.classify.nb

import org.junit.Assert._
import org.junit._
import scala.io._
import opennlp.scalabha.classify._
import opennlp.scalabha.util._
import opennlp.scalabha.util.LogNum._

class NaiveBayesTests {
  def test_noSmoothing() {
    val trainer = OnlineNaiveBayesClassifierTrainer[String, String](0)
    val labeledDocs = new CsvLabeledInstanceSource(
      Source.fromURL(getClass.getResource("/tennis.train.csv")))

    val c = trainer(labeledDocs.getLabeledInstances)

    // verify expected probabilities
    assertEqualsProb(LogNum(9. / 14.), c.priorProb("Yes"))
    assertEqualsProb(LogNum(5. / 14.), c.priorProb("No"))
    assertEqualsProb(LogNum(0. / 20.), c.featureProb("No")("Outlook=Overcast"))
    assertEqualsProb(LogNum(4. / 36.), c.featureProb("Yes")("Outlook=Overcast"))
    assertEqualsProb(LogNum(2. / 20.), c.featureProb("No")("Outlook=Rain"))
    assertEqualsProb(LogNum(3. / 36.), c.featureProb("Yes")("Outlook=Rain"))
    assertEqualsProb(LogNum(3. / 20.), c.featureProb("No")("Outlook=Sunny"))
    assertEqualsProb(LogNum(2. / 36.), c.featureProb("Yes")("Outlook=Sunny"))
    assertEqualsProb(LogNum(1. / 20.), c.featureProb("No")("Temperature=Cool"))
    assertEqualsProb(LogNum(3. / 36.), c.featureProb("Yes")("Temperature=Cool"))
    assertEqualsProb(LogNum(2. / 20.), c.featureProb("No")("Temperature=Hot"))
    assertEqualsProb(LogNum(2. / 36.), c.featureProb("Yes")("Temperature=Hot"))
    assertEqualsProb(LogNum(2. / 20.), c.featureProb("No")("Temperature=Mild"))
    assertEqualsProb(LogNum(4. / 36.), c.featureProb("Yes")("Temperature=Mild"))
    assertEqualsProb(LogNum(4. / 20.), c.featureProb("No")("Humidity=High"))
    assertEqualsProb(LogNum(3. / 36.), c.featureProb("Yes")("Humidity=High"))
    assertEqualsProb(LogNum(1. / 20.), c.featureProb("No")("Humidity=Normal"))
    assertEqualsProb(LogNum(6. / 36.), c.featureProb("Yes")("Humidity=Normal"))
    assertEqualsProb(LogNum(3. / 20.), c.featureProb("No")("Wind=Strong"))
    assertEqualsProb(LogNum(3. / 36.), c.featureProb("Yes")("Wind=Strong"))
    assertEqualsProb(LogNum(2. / 20.), c.featureProb("No")("Wind=Weak"))
    assertEqualsProb(LogNum(6. / 36.), c.featureProb("Yes")("Wind=Weak"))
  }

  def assertEqualsProb(expected: LogNum, model: LogNum) {
    assertEquals(expected.toDouble, model.toDouble, 0.000000000001)
  }
}
