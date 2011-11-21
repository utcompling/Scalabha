package opennlp.scalabha.ccg

object Rule {
  lazy val allRules = List(ForwardApplication, BackwardApplication)
//lazy val allRules = List(ForwardApplication, BackwardApplication, 
//      		   ForwardHarmonicComposition, BackwardHarmonicComposition,
//      		   ForwardCrossedComposition, BackwardCrossedComposition)
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

//object BackwardApplication extends Rule {
//  def apply (first: Cat, second: Cat) = second match {
//    case ComplexCat(res, Left, arg) => if (arg.equals(first)) Some(res) else None
//    case _ => None
//  }
//}

//abstract class Composition(fslash: Slash, sslash: Slash) extends Rule {
//  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat): Option[Cat]
//
//  def apply (first: Cat, second: Cat) = (first, second) match {
//    case (ComplexCat(fres, fslash, farg), ComplexCat(sres, sslash, sarg)) => 
//      getResult(fres, farg, sres, sarg)
//    case _ => None
//  }
//
//}
//
//class ForwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
//  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat) =
//    if (farg.equals(sres)) Some(ComplexCat(fres,Right, sarg)) else None
//}
//
//class BackwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
//  def getResult (fres: Cat, farg: Cat, sres: Cat, sarg: Cat) =
//    if (sarg.equals(fres)) Some(ComplexCat(sres,Right, farg)) else None
//}
//
//object ForwardHarmonicComposition extends ForwardComposition(Right, Right)
//object ForwardCrossedComposition extends ForwardComposition(Right, Left)
//object BackwardHarmonicComposition extends BackwardComposition(Left, Left)
//object BackwardCrossedComposition extends BackwardComposition(Right, Left)

