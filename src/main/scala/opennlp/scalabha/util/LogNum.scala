package opennlp.scalabha.util

import math._

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
  def +[N](other: N)(implicit num: Numeric[N]): LogNum = {
    val oLogValue = log(num.toDouble(other))
    if (logValue == Double.NegativeInfinity)
      new LogNum(oLogValue)
    else if (oLogValue == Double.NegativeInfinity)
      this
    else if (logValue > oLogValue)
      new LogNum(logValue + log1p(exp(oLogValue - logValue)))
    else
      new LogNum(oLogValue + log1p(exp(logValue - oLogValue)))
  }
  def -[N](other: N)(implicit num: Numeric[N]): LogNum = {
    val oLogValue = log(num.toDouble(other))
    if (logValue < oLogValue)
      sys.error("subtraction results in a negative LogNum")
    else if (logValue == 0.0)
      this
    else
      new LogNum(logValue + log1p(-exp(oLogValue - logValue)))
  }
  def *[N](o: N)(implicit num: Numeric[N]): LogNum = new LogNum(logValue + log(num.toDouble(o)))
  def /[N](o: N)(implicit num: Numeric[N]): LogNum = new LogNum(logValue - log(num.toDouble(o)))
  def **[N](pow: N)(implicit num: Numeric[N]): LogNum = new LogNum(num.toDouble(pow) * logValue)

  override def equals(o: Any): Boolean = o match {
    case o: LogNum => logValue == o.logValue
    case o => o == exp(logValue)
  }
  override def hashCode(): Int = toDouble ##

  override def compare(that: LogNum) = logValue.compare(that.logValue)
  def max[N](that: N)(implicit num: Numeric[N]): LogNum = { val thatDouble = log(num.toDouble(that)); if (this.logValue > thatDouble) this else new LogNum(thatDouble) }
  def min[N](that: N)(implicit num: Numeric[N]): LogNum = { val thatDouble = log(num.toDouble(that)); if (this.logValue < thatDouble) this else new LogNum(thatDouble) }

  def approx[N](o: N, tolerance: Double)(implicit num: Numeric[N]): Boolean = {
    if (this == LogNum.zero && o == num.zero)
      true
    else
      (logValue - log(num.toDouble(o))).abs < tolerance
  }
  def approx[N](o: N)(implicit num: Numeric[N]): Boolean = this.approx(o, 0.00000001)

  def toInt = toDouble.toInt
  def toLong = toDouble.toLong
  def toFloat = toDouble.toFloat
  def toDouble = exp(logValue)

  override def toString = "LogNum(%s)".format(toDouble)
}

object LogNum {

  def apply[N](n: N)(implicit num: Numeric[N]) = {
    n match {
      case logNum: LogNum => logNum
      case _ => new LogNum(log(num.toDouble(n)))
    }
  }

  val zero = new LogNum(Double.NegativeInfinity)
  val one = new LogNum(0.0)

  trait LogNumOrdering extends Ordering[LogNum] {
    override def compare(a: LogNum, b: LogNum) = a compare b
  }

  implicit object LogNumIsFractional extends LogNumIsFractional with LogNumOrdering

  trait LogNumIsFractional extends Fractional[LogNum] {
    def plus(x: LogNum, y: LogNum): LogNum = x + y
    def minus(x: LogNum, y: LogNum): LogNum = x - y
    def times(x: LogNum, y: LogNum): LogNum = x * y
    def div(x: LogNum, y: LogNum): LogNum = x / y
    def negate(x: LogNum): LogNum = sys.error("LogNum values cannot be negated")
    def fromInt(x: Int): LogNum = LogNum(x)
    def toInt(x: LogNum): Int = x.toInt
    def toLong(x: LogNum): Long = x.toLong
    def toFloat(x: LogNum): Float = x.toFloat
    def toDouble(x: LogNum): Double = x.toDouble
    override def zero = LogNum.zero
    override def one = LogNum.one
  }

  class EnrichedNumeric[N](self: N)(implicit num: Numeric[N]) {
    def toLogNum = new LogNum(log(num.toDouble(self)))
    def +(n: LogNum) = toLogNum + n
    def -(n: LogNum) = toLogNum - n
    def *(n: LogNum) = toLogNum * n
    def /(n: LogNum) = toLogNum / n
    def **(n: LogNum) = toLogNum ** n
  }
  implicit def enrichNumeric[N: Numeric](self: N) = new EnrichedNumeric(self)

  def pow[N: Numeric](n: LogNum, pow: N): LogNum = n ** pow
  def pow[N: Numeric](n: N, pow: LogNum): LogNum = n ** pow

}
