package opennlp.scalabha.ccg

sealed trait Slash
case object Left  extends Slash { override def toString = "\\" }
case object Right extends Slash { override def toString = "/" }

sealed trait Cat {

  def equals (that: Cat): Boolean

  // Subclasses implement this as appropriate
  def unifies (that: Cat, sub: Substitution): Option[Substitution]

  // Calls the subclass-specific unifies method with an empty map
  def unifies (that: Cat): Option[Substitution] = 
    unifies(that, EmptySubstitution)

  def applySubstitution (sub: Substitution): Cat

  def makeVarsUnique (uniqueIndex: Int, sub: Substitution): (Cat, Int, Substitution)

  val arity: Int
}

object Cat {

  def toStringHandleParens (cat: Cat): String = cat match {
    case ComplexCat(res, slash, arg) => 
      "(" + toStringHandleParens(res) + slash + toStringHandleParens(arg) + ")"

    case ac: AtomCat => 
      ac.toString
  }

  def makeAllVarsUniq (cat1: Cat, cat2: Cat) = {
    val (newCat1, newIndex, _) = cat1.makeVarsUnique(0, EmptySubstitution)
    val (newCat2, _, _) = cat2.makeVarsUnique(newIndex, EmptySubstitution)
    (newCat1, newCat2)
  }

  def makeAllVarsUnique (cats: List[Cat]): List[Cat] = 
    makeAllVarsUnique(cats, 0)

  def makeAllVarsUnique (cats: List[Cat], uniqueIndex: Int): List[Cat] = {
    cats match {
      case Nil => Nil
      case head :: tail => 
        val (newHead, newIndex, newSub) = head.makeVarsUnique(uniqueIndex, EmptySubstitution)
        newHead :: makeAllVarsUnique(tail, newIndex)
    }
  }


}

case class AtomCat (
  val name: String, 
  val features: AttrValMap = EmptyAttrValMap
) extends Cat {

  val arity = 1

  def equals (that: Cat) = that match {
    case ac: AtomCat => name == ac.name
    case _ => false
  }

  def unifies (that: Cat, sub: Substitution) = that match {
    case ac: AtomCat => 
      if (name == ac.name) features.unifies(ac.features, sub) else None
    case _ => None
  }

  def makeVarsUnique (uniqueIndex: Int, sub: Substitution) = {
    val (newFeats, newIndex, newSub) = features.makeVarsUnique(uniqueIndex, sub)
    (AtomCat(name, newFeats), newIndex, newSub)
  }

  def applySubstitution (sub: Substitution) = {
    AtomCat(name, features.applySubstitution(sub))
  }

  override def toString = name+ features
}

case class ComplexCat (val res: Cat, val slash: Slash, val arg: Cat) extends Cat {
  val arity = res.arity + 1

  def equals (that: Cat) = that match {
    case ComplexCat(ores, oslash, oarg) => 
      oslash.equals(slash) && oarg.equals(arg) && ores.equals(res)
    case _ => false
  }

  def unifies (that: Cat, sub: Substitution) = that match {
    case ComplexCat(ores, oslash, oarg) => 
      if (oslash.equals(slash)) 
        oarg.unifies(arg, sub) match {
          case Some(argSub) => ores.unifies(res, sub.add(argSub))
          case None => None
        }
      else
        None

    case _ => 
      None
  }

  def makeVarsUnique (uniqueIndex: Int, sub: Substitution) = {
    val (newRes, newResIndex, newResSub) = res.makeVarsUnique(uniqueIndex, sub)
    val (newArg, newArgIndex, newArgSub) = arg.makeVarsUnique(newResIndex, newResSub)
    (ComplexCat(newRes, slash, newArg), newArgIndex, newArgSub)
  }

  def applySubstitution (sub: Substitution) =
    ComplexCat(res.applySubstitution(sub), slash, arg.applySubstitution(sub))

  override def toString =
    Cat.toStringHandleParens(res) + slash.toString + Cat.toStringHandleParens(arg)

}



object UnifyTest {

  def main (args: Array[String]) {
    val npNom = AtomCat("np", AttrValMap(Map("case" -> Constant("nom"))))
    val npAcc = AtomCat("np", AttrValMap(Map("case" -> Constant("acc"))))
    val npX = AtomCat("np", AttrValMap(Map("case" -> Variable("X"),"num" -> Variable("Z"))))

    val sInd = AtomCat("s", AttrValMap(Map("mood" -> Constant("ind"))))
    val sY = AtomCat("s", AttrValMap(Map("mood" -> Variable("Y"), "case" -> Variable("X"))))

    val intrans1 = ComplexCat(sInd, Left, npX)
    val intrans2 = ComplexCat(sY, Left, npNom)
    val intrans3 = ComplexCat(sY, Left, npX)

    println(npNom.unifies(npAcc))
    println(npNom.unifies(npX))

    val sub = intrans1.unifies(intrans2).get
    println(intrans1.applySubstitution(sub))

    println(intrans3)
    println(npX.makeVarsUnique(0, EmptySubstitution))
    println(intrans3.makeVarsUnique(0, EmptySubstitution))

    val cats = List(intrans1, intrans3)
    println("** " + cats)
    println("-- " + Cat.makeAllVarsUnique(cats))

    val List(uniq1, uniq2) = Cat.makeAllVarsUnique(cats)
    
    println(intrans1.unifies(intrans3))
    println(uniq1.unifies(uniq2))

  }

}

object CatTest {

  def main (args: Array[String]) {
    val s = AtomCat("s")
    val npSg = AtomCat("np", FeatConst("num", "sg"))
    val npPl = AtomCat("np", FeatConst("num", "pl"))
    val sX = AtomCat("s", FeatVar("num", "X"))
    val npX = AtomCat("np", FeatVar("num", "X"))
    val np = AtomCat("np")
    val n = AtomCat("n")
    val adj = ComplexCat(n, Right, n)
    val det = ComplexCat(np, Right, n)
    val intrans = ComplexCat(s, Left, np)
    val intransSg = ComplexCat(s, Left, npSg)
    val intransX = ComplexCat(sX, Left, npX)
    val transSg = ComplexCat(intransSg, Right, np)
    val trnpSg = ComplexCat(s, Right, intransSg)
    
    val lexicon: Map[String,Set[Cat]] = Map(
      "John" -> Set(npSg,trnpSg),
      "Mary" -> Set(npSg,trnpSg),
      "sees" -> Set(transSg),
      "walks" -> Set(intransX),
      "the" -> Set(det),
      "nice" -> Set(adj),
      "dog" -> Set(n)
    )

    //println(intrans + " " + np)
    //println(BackwardApplication(npSg, intransSg).get)
    
    val parser = CkyParser(lexicon)
    println(parser("John sees Mary"))
    println(parser("John walks"))
    //println(parser("John saw the nice dog"))
    //println(parser("John saw"))
  }

}


import util.parsing.combinator._
class CatParser extends JavaTokenParsers {
  
  def apply (catString: String): Cat = parseAll(cat, catString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => throw new RuntimeException(msg)
  }

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

  def acString: Parser[String] = """[a-z][a-z0-9]*""".r
  def constant: Parser[String] = """[a-z][A-Za-z0-9]*""".r
  def varString: Parser[String] = """[A-Z][A-Z0-9]*""".r
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
