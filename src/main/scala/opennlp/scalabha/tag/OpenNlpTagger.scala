package opennlp.scalabha.tag

object OpenNlpTagger {

  import opennlp.tools.sentdetect.SentenceDetectorME
  import opennlp.tools.sentdetect.SentenceModel
  import opennlp.tools.tokenize.TokenizerME
  import opennlp.tools.tokenize.TokenizerModel
  import opennlp.tools.postag.POSTaggerME
  import opennlp.tools.postag.POSModel

  lazy val sentenceDetector = 
    new SentenceDetectorME(
      new SentenceModel(
	this.getClass.getResourceAsStream("/lang/eng/opennlp/en-sent.bin")))

  lazy val tokenizer =
    new TokenizerME(
      new TokenizerModel(
	this.getClass.getResourceAsStream("/lang/eng/opennlp/en-token.bin")))

  lazy val tagger =
    new POSTaggerME(
      new POSModel(
	this.getClass.getResourceAsStream("/lang/eng/opennlp/en-pos-maxent.bin")))

  def main (args: Array[String]) {

    val test = io.Source.fromFile(args(0)).mkString

    println("\n*********************")
    println("Showing sentences.")
    println("*********************")
    val sentences = sentenceDetector.sentDetect(test)
    sentences.foreach(println)

    println("\n*********************")
    println("Showing tokens.")
    println("*********************")
    val tokenizedSentences = sentences.map(tokenizer.tokenize(_))
    tokenizedSentences.foreach(tokens => println(tokens.mkString(" ")))

    println("\n*********************")
    println("Showing POS.")
    println("*********************")
    val postaggedSentences = tokenizedSentences.map(tagger.tag(_))
    postaggedSentences.foreach(postags => println(postags.mkString(" ")))

    println("\n*********************")
    println("Zipping tokens and tags.")
    println("*********************")
    val tokposSentences = 
      tokenizedSentences.zip(postaggedSentences).map { case(tokens, postags) =>
	tokens.zip(postags).map { case(tok,pos) => tok + "/" + pos }
      }
    tokposSentences.foreach(tokposSentence => println(tokposSentence.mkString(" ")))

  }

}
