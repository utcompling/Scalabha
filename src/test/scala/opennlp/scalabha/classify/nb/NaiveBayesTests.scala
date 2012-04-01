package opennlp.scalabha.classify.nb

import scala.io._
import org.scalatest.FunSuite
import opennlp.scalabha.classify._
import opennlp.scalabha.util._

class MockDocument[T](features:Iterable[T]) extends Document[T] {
  def fields = List(("content", features))
}

object MockDocument {
  def apply[T](features:Iterable[T]) = new MockDocument(features)
}

class NaiveBayesTestSuite extends FunSuite {
  test("creates correct online model from tennis.train") {
    val trainer = nb.OnlineNaiveBayesClassifierTrainer[String,String](0)
    val labeledDocs = Source.fromURL(getClass.getResource("/tennis.train.csv"))
                     .getLines.map(line => {
                       val atts = line.split(",")
                       val label = atts(4)
                       val features = atts.slice(0, 4)
                       (label, MockDocument(features))
                     })
    val c = trainer.train(labeledDocs)

    // verify expected probabilities
    assert(c.priorProb("Yes") === LogNum(9./14.))
    assert(c.priorProb("No") === LogNum(5./14.))
    assert(c.featureProb("No")("Outlook=Overcast") === LogNum(0.))
    assert(c.featureProb("Yes")("Outlook=Overcast") === LogNum(4./36.))
    assert(c.featureProb("No")("Outlook=Rain") === LogNum(2./20.))
    assert(c.featureProb("Yes")("Outlook=Rain") === LogNum(3./36.))
    assert(c.featureProb("No")("Outlook=Sunny") === LogNum(3./20.))
    assert(c.featureProb("Yes")("Outlook=Sunny") === LogNum(2./36.))
    assert(c.featureProb("No")("Temperature=Cool") === LogNum(1./20.))
    assert(c.featureProb("Yes")("Temperature=Cool") === LogNum(3./36.))
    assert(c.featureProb("No")("Temperature=Hot") === LogNum(2./20.))
    assert(c.featureProb("Yes")("Temperature=Hot") === LogNum(2./36.))
    assert(c.featureProb("No")("Temperature=Mild") === LogNum(2./20.))
    assert(c.featureProb("Yes")("Temperature=Mild") === LogNum(4./36.))
    assert(c.featureProb("No")("Humidity=High") === LogNum(4./20.))
    assert(c.featureProb("Yes")("Humidity=High") === LogNum(3./36.))
    assert(c.featureProb("No")("Humidity=Normal") === LogNum(1./20.))
    assert(c.featureProb("Yes")("Humidity=Normal") === LogNum(6./36.))
    assert(c.featureProb("No")("Wind=Strong") === LogNum(3./20.))
    assert(c.featureProb("Yes")("Wind=Strong") === LogNum(3./36.))
    assert(c.featureProb("No")("Wind=Weak") === LogNum(2./20.))
    assert(c.featureProb("Yes")("Wind=Weak") === LogNum(6./36.))
  }
}
