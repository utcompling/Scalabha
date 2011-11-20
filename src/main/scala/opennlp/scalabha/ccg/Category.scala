package opennlp.scalabha.ccg

sealed trait Slash
case object Left extends Slash {
  override def toString = "\\"
}

case object Right extends Slash {
  override def toString = "/"
}

sealed trait Cat {
  def equals (that: Cat): Boolean
  val arity: Int
}

object Cat {

  def toStringHandleParens (cat: Cat): String = cat match {
    case ComplexCat(res, slash, arg) => "(" + toStringHandleParens(res) + slash + toStringHandleParens(arg) + ")"
    case ac: AtomCat => ac.toString
  }


}

case class AtomCat (val name: String) extends Cat {
  val arity = 1

  def equals (that: Cat) = that match {
    case ac: AtomCat => name == ac.name
    case _ => false
  }

  override def toString = name
}

case class ComplexCat (val res: Cat, val slash: Slash, val arg: Cat) extends Cat {
  val arity = res.arity + 1

  def equals (that: Cat) = that match {
    case ComplexCat(ores, oslash, oarg) => oslash.equals(slash) && oarg.equals(arg) && ores.equals(res)
    case _ => false
  }

  override def toString =
    Cat.toStringHandleParens(res) + slash.toString + Cat.toStringHandleParens(arg)

}



object CatTest {

  def main (args: Array[String]) {
    val s = AtomCat("s")
    val np = AtomCat("np")
    val n = AtomCat("n")
    val adj = ComplexCat(n, Right, n)
    val det = ComplexCat(np, Right, n)
    val intrans = ComplexCat(s, Left, np)
    val trans = ComplexCat(intrans, Right, np)
    val trnp = ComplexCat(s, Right, intrans)
    
    val lexicon: Map[String,Set[Cat]] = Map(
      "John" -> Set(np,trnp),
      "Mary" -> Set(np,trnp),
      "saw" -> Set(trans),
      "the" -> Set(det),
      "nice" -> Set(adj),
      "dog" -> Set(n)
    )

    //println(intrans + " " + np)
    //println(BackwardApplication(np, intrans).get)
    
    val parser = CkyParser(lexicon)
    println(parser("John saw Mary"))
    println(parser("John saw the nice dog"))
    println(parser("John saw"))

  }

}
