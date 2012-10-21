package opennlp.scalabha.util

import scala.collection.generic.CanBuildFrom

object Pattern {

  object UnapplyInt {
    def x = 1
    val IntRE = """^(-?\d+)$""".r
    def unapply(v: String): Option[Int] = v match {
      case IntRE(s) => Some(s.toInt)
      case _ => None
    }
  }
  //  implicit def int2unapplyInt(objA: Int.type) = UnapplyInt

  object UnapplyDouble {
    val DoubleRE = """^(-?\d+\.?\d*|-?\d*\.?\d+)$""".r
    def unapply(v: String): Option[Double] = v match {
      case DoubleRE(s) => Some(s.toDouble)
      case _ => None
    }
  }
  //  implicit def double2unapplyDouble(objA: Double.type) = UnapplyDouble

  object Map {
    def unapplySeq[A, B](m: Map[A, B]): Option[Seq[(A, B)]] = Some(m.toIndexedSeq)
  }

  object Set {
    def unapplySeq[A](s: Set[A]): Option[Seq[A]] = Some(s.toIndexedSeq)
  }

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = {
      Some(pair)
    }
  }

  object Range {
    val RangeRE = """^(\d+)-(\d+)$""".r
    def unapply(s: String): Option[Seq[Int]] = Some(
      s.replaceAll("\\s+", "").split(",").flatMap {
        case UnapplyInt(i) => i to i
        case RangeRE(UnapplyInt(b), UnapplyInt(e)) if b <= e => b to e
      })
  }

  class Range(max: Int) {
    val OpenRangeRE = """^(\d+)-$""".r
    def unapply(s: String): Option[Seq[Int]] = Some(
      s.replaceAll("\\s+", "").split(",").flatMap {
        case OpenRangeRE(UnapplyInt(b)) => b to max
        case Range(r) => r
      })
  }

  def makeRangeString(seq: Seq[Int]): String = {
    assert(seq.nonEmpty, "cannot make empty sequence into a range string")
    assert(seq.exists(_ >= 0), "negative numbers are not permitted: %s".format(seq))
    (-2 +: seq).sliding(2).foldLeft(Vector[Vector[Int]]()) {
      case ((z :+ c), Seq(a, b)) =>
        if (a != b - 1)
          (z :+ c) :+ Vector(b)
        else
          (z :+ (c :+ b))
      case (z, Seq(a, b)) =>
        z :+ Vector(b)
    }
      .map {
        case Seq(x) => x.toString
        case s => s.head + "-" + s.last
      }.mkString(",")
  }

  object +: {
    def unapply[CC, A, That](seq: CC)(implicit asSeq: CC => Seq[A], cbf: CanBuildFrom[CC, A, That]): Option[(A, That)] = {
      if (seq.nonEmpty)
        Some(seq.head, cbf(seq) ++= seq.tail result)
      else
        None
    }
  }

  object :+ {
    def unapply[CC, A, That](seq: CC)(implicit asSeq: CC => Seq[A], cbf: CanBuildFrom[CC, A, That]): Option[(That, A)] = {
      if (seq.nonEmpty)
        Some(cbf(seq) ++= seq.dropRight(1) result, seq.last)
      else
        None
    }
  }

  object Iterable {
    def unapplySeq[T](s: Iterable[T]): Option[Seq[T]] =
      Some(s.toIndexedSeq)
  }

}
