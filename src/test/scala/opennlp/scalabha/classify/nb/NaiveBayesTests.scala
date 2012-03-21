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
    assert(c.featureProb("Outlook=Overcast", "No") === LogNum(0.))
    assert(c.featureProb("Outlook=Overcast", "Yes") === LogNum(4./36.))
    assert(c.featureProb("Outlook=Rain", "No") === LogNum(2./20.))
    assert(c.featureProb("Outlook=Rain", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Outlook=Sunny", "No") === LogNum(3./20.))
    assert(c.featureProb("Outlook=Sunny", "Yes") === LogNum(2./36.))
    assert(c.featureProb("Temperature=Cool", "No") === LogNum(1./20.))
    assert(c.featureProb("Temperature=Cool", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Temperature=Hot", "No") === LogNum(2./20.))
    assert(c.featureProb("Temperature=Hot", "Yes") === LogNum(2./36.))
    assert(c.featureProb("Temperature=Mild", "No") === LogNum(2./20.))
    assert(c.featureProb("Temperature=Mild", "Yes") === LogNum(4./36.))
    assert(c.featureProb("Humidity=High", "No") === LogNum(4./20.))
    assert(c.featureProb("Humidity=High", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Humidity=Normal", "No") === LogNum(1./20.))
    assert(c.featureProb("Humidity=Normal", "Yes") === LogNum(6./36.))
    assert(c.featureProb("Wind=Strong", "No") === LogNum(3./20.))
    assert(c.featureProb("Wind=Strong", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Wind=Weak", "No") === LogNum(2./20.))
    assert(c.featureProb("Wind=Weak", "Yes") === LogNum(6./36.))
  }

  test("creates correct compiled online model from tennis.train") {
    val trainer = nb.OnlineNaiveBayesClassifierTrainer[String,String](0)
    val labeledDocs = Source.fromURL(getClass.getResource("/tennis.train.csv"))
                     .getLines.map(line => {
                       val atts = line.split(",")
                       val label = atts(4)
                       val features = atts.slice(0, 4)
                       (label, MockDocument(features))
                     })
    val c = trainer.train(labeledDocs).compiled

    // verify expected probabilities
    assert(c.priorProb("Yes") === LogNum(9./14.))
    assert(c.priorProb("No") === LogNum(5./14.))
    assert(c.featureProb("Outlook=Overcast", "No") === LogNum(0.))
    assert(c.featureProb("Outlook=Overcast", "Yes") === LogNum(4./36.))
    assert(c.featureProb("Outlook=Rain", "No") === LogNum(2./20.))
    assert(c.featureProb("Outlook=Rain", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Outlook=Sunny", "No") === LogNum(3./20.))
    assert(c.featureProb("Outlook=Sunny", "Yes") === LogNum(2./36.))
    assert(c.featureProb("Temperature=Cool", "No") === LogNum(1./20.))
    assert(c.featureProb("Temperature=Cool", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Temperature=Hot", "No") === LogNum(2./20.))
    assert(c.featureProb("Temperature=Hot", "Yes") === LogNum(2./36.))
    assert(c.featureProb("Temperature=Mild", "No") === LogNum(2./20.))
    assert(c.featureProb("Temperature=Mild", "Yes") === LogNum(4./36.))
    assert(c.featureProb("Humidity=High", "No") === LogNum(4./20.))
    assert(c.featureProb("Humidity=High", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Humidity=Normal", "No") === LogNum(1./20.))
    assert(c.featureProb("Humidity=Normal", "Yes") === LogNum(6./36.))
    assert(c.featureProb("Wind=Strong", "No") === LogNum(3./20.))
    assert(c.featureProb("Wind=Strong", "Yes") === LogNum(3./36.))
    assert(c.featureProb("Wind=Weak", "No") === LogNum(2./20.))
    assert(c.featureProb("Wind=Weak", "Yes") === LogNum(6./36.))
  }
}
