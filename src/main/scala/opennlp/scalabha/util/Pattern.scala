package opennlp.scalabha.util

object Pattern {

  object Int {
    val IntRE = """^(\d+)$""".r
    def unapply(v: Any): Option[Int] = v match {
      case i: Int => Some(i)
      case s: String => s match { case IntRE(i) => Some(i.toInt); case _ => None }
    }
  }

  object Double {
    val DoubleRE = """^(\d+\.?\d*)|(\d*\.?\d+)$""".r
    def unapply(v: Any): Option[Double] = v match {
      case i: Int => Some(i)
      case l: Long => Some(l)
      case f: Float => Some(f)
      case d: Double => Some(d)
      case s: String => if (DoubleRE.findFirstIn(s).isDefined) Some(s.toDouble) else None
    }
  }

  object Map {
    def unapplySeq[A, B](m: Map[A, B]): Option[Seq[(A, B)]] = Some(m.toList)
  }

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] = {
      val (a, b) = pair
      Some(a -> b)
    }
  }

  object Range {
    val RangeRE = """^(\d+)-(\d*)$""".r
    def unapplySeq(s: String): Option[Seq[Int]] = Some((
      s.split(",").flatMap {
        case Int(i) => i.toInt to i.toInt
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
    def unapply[T](s: Iterable[T]): Option[(T, Iterable[T])] =
      if (s.size >= 1) {
        val itr = s.iterator
        Some(itr.next, new Iterable[T] { def iterator = itr })
      }
      else
        None
  }

  object :+ {
    def unapply[T](s: Iterable[T]): Option[(Iterable[T], T)] =
      if (s.size >= 1)
        Some(s.dropRight(1), s.last)
      else
        None
  }

  object Iterable {
    def unapplySeq[T](s: Iterable[T]): Option[Seq[T]] =
      Some(s.toSeq)
  }

}
