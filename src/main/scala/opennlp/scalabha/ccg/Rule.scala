package opennlp.scalabha.ccg

object Rule {
  //lazy val allRules = List(ForwardApplication, BackwardApplication)

  lazy val abRules = List(ForwardApplication, BackwardApplication)
  lazy val harmonicRules = abRules ::: List(ForwardHarmonicComposition, BackwardHarmonicComposition)
  lazy val allRules = harmonicRules ::: List(ForwardCrossedComposition, BackwardCrossedComposition)
  lazy val englishRules = harmonicRules ::: List(BackwardCrossedComposition)
}

trait Rule {
  def apply (first: Cat, second: Cat): Option[Cat]
}

object ForwardApplication extends Rule {
  def apply (first: Cat, second: Cat) = first match {
    case ComplexCat(res, Right, arg) => 
      arg.unifies(second) match {
        case Some(sub) => Some(res.applySubstitution(sub))
        case None => None
      }

    case _ => None
  }
}

object BackwardApplication extends Rule {
  def apply (first: Cat, second: Cat) = second match {
    case ComplexCat(res, Left, arg) => 
      arg.unifies(first) match {
        case Some(sub) => Some(res.applySubstitution(sub))
        case None => None
      }
    case _ => None
  }
}

abstract class Composition(fslash: Slash, sslash: Slash) extends Rule {
  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat): Option[Cat]

  def apply (first: Cat, second: Cat) = (first, second) match {
    case (ComplexCat(fres, fslash, farg), ComplexCat(sres, sslash, sarg)) => 
      getResult(fres, farg, sres, sarg)
    case _ => None
  }

}

class ForwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat) =
    farg.unifies(sres) match {
      case Some(sub) => 
        val subbedFres = fres.applySubstitution(sub) 
        val subbedSarg = sarg.applySubstitution(sub) 
        Some(ComplexCat(subbedFres, Right, subbedSarg))

      case None => None
    }
}

class BackwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat) =
    sarg.unifies(fres) match {
      case Some(sub) => 
        val subbedSres = sres.applySubstitution(sub) 
        val subbedFarg = farg.applySubstitution(sub) 
        Some(ComplexCat(subbedSres, Left, subbedFarg))

      case None => None
    }

}

object ForwardHarmonicComposition extends ForwardComposition(Right, Right)
object ForwardCrossedComposition extends ForwardComposition(Right, Left)
object BackwardHarmonicComposition extends BackwardComposition(Left, Left)
object BackwardCrossedComposition extends BackwardComposition(Right, Left)

