package opennlp.scalabha.ccg


/** A slash in a CCG category. */
sealed trait Slash

/** A left slash in a CCG category. */
case object Left  extends Slash { override def toString = "\\" }

/** A right slash in a CCG category. */
case object Right extends Slash { override def toString = "/" }

/** A trait for CCG categories. */
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

/** A companion object for the Cat trait. Contains a number of helper methods. */
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

/**
 * An atomic category, like np, s and n. May contain features,
 * e.g. np[case=nom] and s[mood=ind].
 */
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

/** A complex category, like s\np and (n\n)/(s/np) */
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




