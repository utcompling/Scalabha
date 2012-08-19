package opennlp.scalabha.util

final class MutableDouble(private var value: Double) extends Ordered[MutableDouble] {
  def +[N](other: N)(implicit num: Numeric[N]) = value + num.toDouble(other)
  def -[N](other: N)(implicit num: Numeric[N]) = value - num.toDouble(other)
  def *[N](other: N)(implicit num: Numeric[N]) = value * num.toDouble(other)
  def /[N](other: N)(implicit num: Numeric[N]) = value / num.toDouble(other)
  
  def unary_-[N](implicit num: Numeric[N]) = -value

  def +=[N](other: N)(implicit num: Numeric[N]) = { value += num.toDouble(other); this }
  def -=[N](other: N)(implicit num: Numeric[N]) = { value -= num.toDouble(other); this }
  def *=[N](other: N)(implicit num: Numeric[N]) = { value *= num.toDouble(other); this }
  def /=[N](other: N)(implicit num: Numeric[N]) = { value /= num.toDouble(other); this }

  override def equals(o: Any): Boolean = o == value
  override def hashCode(): Int = value ##

  override def compare(that: MutableDouble) = value compare that.value
  def max[N](other: N)(implicit num: Numeric[N]) = if (value > num.toDouble(other)) this else MutableDouble(other)
  def min[N](other: N)(implicit num: Numeric[N]) = if (value < num.toDouble(other)) this else MutableDouble(other)

  def toInt = value.toInt
  def toLong = value.toLong
  def toFloat = value.toFloat
  def toDouble = value

  override def toString = "MutableDouble(%s)".format(toDouble)
}

object MutableDouble {

  def apply[N](n: N)(implicit num: Numeric[N]) = {
    n match {
      case mutableDouble: MutableDouble => mutableDouble
      case _ => new MutableDouble(num.toDouble(n))
    }
  }

  val zero = new MutableDouble(0.0)
  val one = new MutableDouble(1.0)

  trait MutableDoubleOrdering extends Ordering[MutableDouble] {
    override def compare(a: MutableDouble, b: MutableDouble) = a compare b
  }

  implicit object MutableDoubleIsFractional extends MutableDoubleIsFractional with MutableDoubleOrdering

  trait MutableDoubleIsFractional extends Fractional[MutableDouble] {
    def plus(x: MutableDouble, y: MutableDouble): MutableDouble = new MutableDouble(x + y)
    def minus(x: MutableDouble, y: MutableDouble): MutableDouble = new MutableDouble(x - y)
    def times(x: MutableDouble, y: MutableDouble): MutableDouble = new MutableDouble(x * y)
    def div(x: MutableDouble, y: MutableDouble): MutableDouble = new MutableDouble(x / y)
    def negate(x: MutableDouble): MutableDouble = new MutableDouble(-x)
    def fromInt(x: Int): MutableDouble = new MutableDouble(x)
    def toInt(x: MutableDouble): Int = x.toInt
    def toLong(x: MutableDouble): Long = x.toLong
    def toFloat(x: MutableDouble): Float = x.toFloat
    def toDouble(x: MutableDouble): Double = x.toDouble
    override def zero = MutableDouble.zero
    override def one = MutableDouble.one
  }

  class EnrichedNumeric[N](self: N)(implicit num: Numeric[N]) {
    def +(n: MutableDouble) = num.toDouble(self) + n.value
    def -(n: MutableDouble) = num.toDouble(self) - n.value
    def *(n: MutableDouble) = num.toDouble(self) * n.value
    def /(n: MutableDouble) = num.toDouble(self) / n.value
  }
  implicit def enrichNumeric[N: Numeric](self: N) = new EnrichedNumeric(self)

}
