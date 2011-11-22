package opennlp.scalabha.ccg

case class LexicalEntry (word: String, cat: Cat)

object Lexicon {

  lazy val catParser = new CatParser

  def apply (entries: List[String]) = {
    val lentries = entries.map(entry => catParser.parseLexEntry(entry))
    lentries.groupBy(_.word).mapValues {
      entries => entries.map(_.cat).toSet
    } 
  }

}

object LexiconTest {
  def main (args: Array[String]) {
    Lexicon(io.Source.fromFile(args(0)).getLines.toList)
  }
}

import util.parsing.combinator._

class CatParser extends JavaTokenParsers {
  
  def apply (catString: String): Cat = parseAll(cat, catString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => throw new RuntimeException(msg)
  }

  def parseLexEntry (entryString: String): LexicalEntry = parseAll(lexEntry, entryString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => {
      System.err.println("\nCouldn't process the following entry:\n\n" + entryString 
			 + "\n\nError: "  + msg + "\n")
      throw new RuntimeException()
      //System.exit(0)
    }
  }

  def lexEntry: Parser[LexicalEntry] = 
    word ~ ":=" ~ cat ^^ { case w ~ ":=" ~ c => LexicalEntry(w,c) }

  def cat: Parser[Cat] = (complexCat | atomCat )

  def innerCat: Parser[Cat] = (atomCat | "(" ~> complexCat <~ ")")

  def atomCat: Parser[AtomCat] = 
    acString ~ opt(attrValMap) ^^ { 
      case ac ~ None => AtomCat(ac)
      case ac ~ Some(av) => AtomCat(ac, av)
    }
     

  def complexCat: Parser[ComplexCat] = 
    innerCat ~ slash ~ innerCat ^^ { case r~s~a => ComplexCat(r,s,a) }

  def slash: Parser[Slash] = (
      "\\" ^^ (x => Left)
    | "/"  ^^ (x => Right)
  )

  def attrValMap: Parser[AttrValMap] = 
    "[" ~> repsep(feature,",") <~ "]" ^^ (_.fold(AttrValMap(Map()))(_ ++ _))

  def feature: Parser[AttrValMap] =
    ( constant ~ "=" ~ constant ^^ { case a~"="~v => FeatConst(a,v) }
     | constant ~ "=" ~ varString ^^ { case a~"="~v => FeatVar(a,v) }
   )

  def acString: Parser[String] = """[a-z][a-z0-9_]*""".r
  def constant: Parser[String] = """[a-z][A-Za-z0-9_]*""".r
  def varString: Parser[String] = """[A-Z][A-Z0-9_]*""".r
  def word: Parser[String] = """[^\s]+""".r
}

object InputTest {
  def main (args: Array[String]) {
    val cparser = new CatParser
    println(cparser("np"))
    println(cparser("""(s\np)/np"""))
    println(cparser("""(s\np)/(s/np)"""))
    println(cparser("np[foo=bar]"))
    println(cparser("np[foo=X,bar=baz]"))

    val intransSg = cparser("""s\np[num=sg]""")
    val npSg = cparser("""np[num=sg]""")
    println(BackwardApplication(npSg, intransSg).get)

  }
}
