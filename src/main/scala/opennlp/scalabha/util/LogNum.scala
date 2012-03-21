package opennlp.scalabha.util

/**
 * This Numeric class represents values using logarithms.  The underlying 
 * logarithmic representation is completely hidden from the calling code.
 *
 * This class exists to allow for the use of obvious operators (* for
 * multiplication instead of + on logarithms) and to prevent coding mistakes
 * resulting from the inadvertent mixing of logarithmic and non-logarithmic
 * Double representations of probabilities.  Additionally, it is possible to 
 * use the `sum` and `product` collection methods on collections of 
 * Probabilities, and get the expected results.
 *
 * All to* methods return the (non-logarithmic) value stored.  The only
 * way to access the actual logarithmic value is by the 'logValue' field.
 */
final class LogNum(val logValue: Double) extends Ordered[LogNum] {
  def +(o: LogNum): LogNum = {
    if (logValue == Double.NegativeInfinity)
      o
    else if (o.logValue == Double.NegativeInfinity)
      this
    else if (logValue > o.logValue)
      new LogNum(logValue + math.log(1 + math.exp(o.logValue - logValue)))
    else
      new LogNum(o.logValue + math.log(1 + math.exp(logValue - o.logValue)))
  }
  def *(o: LogNum): LogNum = new LogNum(logValue + o.logValue)
  def /(o: LogNum): LogNum = new LogNum(logValue - o.logValue)

  override def equals(o: Any): Boolean = o match {
    case o: LogNum => logValue == o.logValue
    case _ => false
  }
  override def hashCode(): Int = toDouble ##
  
  override def compare(that: LogNum) = logValue.compare(that.logValue)
  def approx(o: LogNum, tolerance: Double): Boolean = {
    if(this == LogNum.zero && o == LogNum.zero)
      true
    else
      (logValue - o.logValue).abs < tolerance
  }
  def approx(o: LogNum): Boolean = this.approx(o, 0.00000001)

  def toInt = toDouble.toInt
  def toLong = toDouble.toLong
  def toFloat = toDouble.toFloat
  def toDouble = math.exp(logValue)
  
  override def toString = "LogNum(%s)".format(toDouble)
}

object LogNum {

  def apply(d: Double) = new LogNum(math.log(d))

  val zero = new LogNum(Double.NegativeInfinity)
  val one = new LogNum(0.0)

  trait LogNumOrdering extends Ordering[LogNum] {
    override def compare(a: LogNum, b: LogNum) = a compare b
  }

  class EnrichedNumeric[N](self: N)(implicit num: Numeric[N]) {
    def toLogNum = LogNum(num.toDouble(self))
  }
  implicit def enrichNumeric(self: Double) = new EnrichedNumeric(self)

  trait LogNumIsFractional extends Fractional[LogNum] {
    def plus(x: LogNum, y: LogNum): LogNum = x + y
    def minus(x: LogNum, y: LogNum): LogNum = sys.error("not implemented")
    def times(x: LogNum, y: LogNum): LogNum = x * y
    def div(x: LogNum, y: LogNum): LogNum = x / y
    def negate(x: LogNum): LogNum = sys.error("LogNum values cannot be negated")
    def fromInt(x: Int): LogNum = x.toDouble.toLogNum
    def toInt(x: LogNum): Int = x.toInt
    def toLong(x: LogNum): Long = x.toLong
    def toFloat(x: LogNum): Float = x.toFloat
    def toDouble(x: LogNum): Double = x.toDouble
  }

  implicit object LogNumIsFractional extends LogNumIsFractional with LogNumOrdering

}
