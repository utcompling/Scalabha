package opennlp.scalabha.ccg

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
