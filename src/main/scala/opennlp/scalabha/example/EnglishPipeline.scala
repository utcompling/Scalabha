package opennlp.scalabha.example

object EnglishTools {

  import opennlp.tools.sentdetect.SentenceDetectorME
  import opennlp.tools.sentdetect.SentenceModel
  import opennlp.tools.tokenize.TokenizerME
  import opennlp.tools.tokenize.TokenizerModel
  import opennlp.tools.postag.POSTaggerME
  import opennlp.tools.postag.POSModel
  import opennlp.tools.chunker.ChunkerME
  import opennlp.tools.chunker.ChunkerModel
  import opennlp.tools.namefind.NameFinderME
  import opennlp.tools.namefind.TokenNameFinderModel

  private def getEngResource(modelName: String) =
    this.getClass.getResourceAsStream("/lang/eng/opennlp/" + modelName)

  lazy val sentenceDetector =
    new SentenceDetectorME(new SentenceModel(getEngResource("en-sent.bin")))

  lazy val tokenizer =
    new TokenizerME(new TokenizerModel(getEngResource("en-token.bin")))

  lazy val tagger =
    new POSTaggerME(new POSModel(getEngResource("en-pos-maxent.bin")))

  lazy val chunker =
    new ChunkerME(new ChunkerModel(getEngResource("en-chunker.bin")))

  lazy val personFinder =
    new NameFinderME(new TokenNameFinderModel(getEngResource("en-ner-person.bin")))

}

trait LanguagePipeline {
  def getSentences(text: String): Array[String]
}

object EnglishPipeline extends LanguagePipeline {


  import EnglishTools._
  import opennlp.tools.util.Span

  class TokenArray(tokens: Array[String]) {
    def substring(span: Span) = 
      tokens.slice(span.getStart, span.getEnd).mkString(" ")
  }

  implicit def arrayStringToTokenArray(tokens: Array[String]) =
    new TokenArray(tokens)

  def getSentences(text: String) = sentenceDetector.sentDetect(text)

  def getChunks(text: String, chunkType: String = "NP") = {
    getSentences(text).flatMap { 
      sentence => {
	val tokens = tokenizer.tokenize(sentence)
	chunker.chunkAsSpans(tokens, tagger.tag(tokens)) 
	  .filter(_.getType == chunkType)
          .map(chunkSpan => tokens.substring(chunkSpan))
      }		   
    }
  }

  def getPersons(text: String) = {
    sentenceDetector.sentDetect(text).flatMap { 
      sentence => {
	val tokens = tokenizer.tokenize(sentence)
	personFinder.find(tokens)
          .map(personSpan => tokens.substring(personSpan))
      }		   
					      
    }
  }

  def main(args: Array[String]) {

    val test = io.Source.fromFile(args(0)).mkString

    println("\n*********************")
    println("Showing sentences.")
    println("*********************")
    val sentences: Array[String] = sentenceDetector.sentDetect(test)
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
      tokenizedSentences.zip(postaggedSentences).map {
        case (tokens, postags) =>
          tokens.zip(postags).map { case (tok, pos) => tok + "/" + pos }
      }
    tokposSentences.foreach(tokposSentence => println(tokposSentence.mkString(" ")))

    println("\n*********************")
    println("Chunking.")
    println("*********************")
    val chunkedSentences = tokenizedSentences.zip(postaggedSentences).map {
        case (tokens, postags) => chunker.chunkAsSpans(tokens, postags)
      }

    val chunkSequences = tokenizedSentences.zip(chunkedSentences).map {
      case (tokens, chunks) =>
        chunks
          .filter(_.getType == "NP")
          .map(chunk => tokens.slice(chunk.getStart, chunk.getEnd).mkString(" ") )
    }

    chunkSequences.foreach { chunksForSentence =>
      println("***")
      println(chunksForSentence.mkString("\n"))
    }

    println("\n*********************")
    println("Showing NER.")
    println("*********************")
    val personTaggedSentences = tokenizedSentences.map(personFinder.find(_))
    val persons = tokenizedSentences.zip(personTaggedSentences).map {
      case (tokens, personTags) =>
        personTags.map(ptag => tokens.slice(ptag.getStart, ptag.getEnd).mkString(" "))
    }
    persons.foreach { personsForSentence =>
      println("***")
      println(personsForSentence.mkString("\n"))
    }
    
   
    
  }

}
