package opennlp.scalabha.ccg

import util.parsing.combinator._

/**
 * A parser combinator that turns strings describing categories
 * and lexical items into their corresponding
 * opennlp.scalabha.ccg objects.
 *
 * Note: "parser" here is different from a CkyParser, which is
 * analyzing natural language strings using a given CCG lexicon.
 * Here, expressions like (s\np)/np are "parsed" to create the
 * objects (e.g. AtomCats, Slashes, and ComplexCats) described
 * by the expression.
 */
class CatParser extends JavaTokenParsers {
  
  // Turn a category string (like "s\np") into a Cat object.
  def apply (catString: String): Cat = parseAll(cat, catString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => throw new RuntimeException(msg)
  }

  // Turn a lexical entry (like "walks := s\np") into a LexicalEntry object.
  def parseLexEntry (entryString: String): LexicalEntry = parseAll(lexEntry, entryString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => {
      throw new CatParserException("\n\nCouldn't process the following entry:\n\n" + entryString 
				   + "\n\nError: "  + msg + "\n")
    }
  }

  // The remaining methods specify the production rules of the
  // context-free grammar that defines categories and lexical entries.

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

  def feature: Parser[AttrValMap] = ( 
      constant ~ "=" ~ constant ^^ { case a~"="~v => FeatConst(a,v) }
    | constant ~ "=" ~ varString ^^ { case a~"="~v => FeatVar(a,v) }
  )

  def acString: Parser[String] = """[a-z][a-z0-9_]*""".r
  def constant: Parser[String] = """[a-z0-9][A-Za-z0-9_]*""".r
  def varString: Parser[String] = """[A-Z][A-Z0-9_]*""".r
  def word: Parser[String] = """[^\s]+""".r

}

class CatParserException (msg: String) extends Throwable(msg) {
  override def fillInStackTrace = this
}
