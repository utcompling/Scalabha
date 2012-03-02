package opennlp.scalabha.cluster

// A simple representation of a point in some n-dimensional space.
case class Point(val coord: IndexedSeq[Double]) {
  import scala.math.sqrt

  def zip(that: Point) = this.coord.zip(that.coord)
  def ++(that: Point) = Point(this.zip(that).map { case (a, b) => a + b })
  def /(divisor: Double) = Point(coord.map(_ / divisor))
  def -(that: Point) = Point(this.zip(that).map { case (a, b) => a - b })
  def dotProduct(that: Point) = this.zip(that).map { case (x, y) => x * y }.sum

  lazy val abs = Point(coord.map(_.abs))
  lazy val norm = sqrt(this.dotProduct(this))
  lazy val numDimensions = coord.length
  lazy val sum = coord.sum

  override def toString = "[" + coord.mkString(",") + "]"
}

// A class for objects that transform Points to and from z-score
// values based on means and standard deviations in each dimension.
class ZscoreTransformer(
  means: IndexedSeq[Double], standardDeviations: IndexedSeq[Double]) {

  // Transform a point to its z-score values.
  def to(point: Point) = {
    val transformed = point.coord.zip(means.zip(standardDeviations)).map {
      case (x, (mean, sdev)) => (x - mean) / sdev
    }
    Point(transformed)
  }

  // Transform a point from the z-scores value back to its original ones.
  def from(point: Point) = {
    val untransformed = point.coord.zip(means.zip(standardDeviations)).map {
      case (z, (mean, sdev)) => z * sdev + mean
    }
    Point(untransformed)
  }

}

// Companion object that computes means and standard deviations to
// construct a ZscoreTransformer.
object ZscoreTransformer {

  def apply(points: IndexedSeq[Point]) = {
    val tpoints = points.map(_.coord).transpose
    val means = tpoints.map(values => values.sum / values.length)
    val standardDeviations = tpoints.zip(means).map {
      case (values, mean) =>
        val squaredDifferences = values.map { value =>
          val difference = value - mean
          difference * difference
        }
        math.sqrt(squaredDifferences.sum)
    }
    new ZscoreTransformer(means, standardDeviations)

  }

}

abstract class DistanceFunction extends ((Point, Point) => Double)

object DistanceFunction {
  def apply(distanceDescription: String) = distanceDescription match {
    case "cosine" => CosineDistance
    case "manhattan" => ManhattanDistance
    case "euclidean" => EuclideanDistance
    case _ =>
      throw new MatchError("Invalid distance function: " + distanceDescription)
  }
}

object CosineDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = 1 - x.dotProduct(y) / (x.norm * y.norm)
}

object ManhattanDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = (x - y).abs.sum
}

object EuclideanDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = (x - y).norm
}
