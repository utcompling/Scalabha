package opennlp.scalabha.util
import scala.collection.generic.CanBuildFrom

object Pattern {

  object UnapplyInt {
    def x = 1
    val IntRE = """^(\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }
  //  implicit def int2unapplyInt(objA: Int.type) = UnapplyInt

  object UnapplyDouble {
    val DoubleRE = """^(\d+\.?\d*|\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }
  //  implicit def double2unapplyDouble(objA: Double.type) = UnapplyDouble

  object Map {
    def unapplySeq[A, B](m: Map[A, B]): Option[Seq[(A, B)]] = Some(m.toList)
  }

  object Set {
    def unapplySeq[A](s: Set[A]): Option[Seq[A]] = Some(s.toList)
  }

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = {
      Some(pair)
    }
  }

  object Range {
    val RangeRE = """^(\d+)-(\d*)$""".r
    def unapplySeq(s: String): Option[Seq[Int]] = Some((
      s.split(",").flatMap {
        case UnapplyInt(i) => i.toInt to i.toInt
        case RangeRE(b, e) => b.toInt to e.toInt
      }).toSet.toList.sorted)
  }

  class Range(max: Int) {
    val OpenRangeRE = """^(\d+)-$""".r
    def unapplySeq(s: String): Option[Seq[Int]] = Some((
      s.split(",").flatMap {
        case OpenRangeRE(b) => b.toInt to max
        case Range(r @ _*) => r
      }).toSet.toList.sorted)
  }

  object +: {
    def unapply[CC, A, That](seq: CC)(implicit asSeq: CC => Seq[A], cbf: CanBuildFrom[CC, A, That]): Option[(A, That)] = {
      if (seq.size >= 1) {
        val b = cbf(seq)
        b ++= seq.tail
        Some(seq.head, b.result)
      }
      else
        None
    }
  }

  object :+ {
    def unapply[CC, A, That](seq: CC)(implicit asSeq: CC => Seq[A], cbf: CanBuildFrom[CC, A, That]): Option[(That, A)] = {
      if (seq.size >= 1) {
        val b = cbf(seq)
        b ++= seq.dropRight(1)
        Some(b.result, seq.last)
      }
      else
        None
    }
  }

  object Iterable {
    def unapplySeq[T](s: Iterable[T]): Option[Seq[T]] =
      Some(s.toSeq)
  }

}
