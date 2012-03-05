package opennlp.scalabha.cluster

import pca_transform.PCA
import Jama.Matrix

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

trait PointTransformer extends (IndexedSeq[Point] => IndexedSeq[Point])

object PointTransformer {
  def apply(description: String, points: IndexedSeq[Point]) = description match {
    case "ident" => new IdentityTransformer
    case "zscore" => ZscoreTransformer(points)
    case "pca" => PcaTransformer(points)
  }
}

class IdentityTransformer extends PointTransformer {
  def apply(points: IndexedSeq[Point]) = points
}

// A class for objects that transform Points to and from z-score
// values based on means and standard deviations in each dimension.
class ZscoreTransformer(
  means: IndexedSeq[Double], standardDeviations: IndexedSeq[Double])
  extends PointTransformer {

  // Transform a point to its z-score values.
  def apply(points: IndexedSeq[Point]) = {
    points.map { point =>
      val transformed = point.coord.zip(means.zip(standardDeviations)).map {
        case (x, (mean, sdev)) => (x - mean) / sdev
      }
      Point(transformed)
    }
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
        val squaredDifferences = values.map(v => square(v-mean))
        if (squaredDifferences == 0.0) 1.0
        else math.sqrt(squaredDifferences.sum)
    }
    println(means)
    println(standardDeviations)
    new ZscoreTransformer(means, standardDeviations)
  }

  private def square = (x: Double) => x*x 
}

class PcaTransformer(
  pca: PCA,
  scaler: ZscoreTransformer,
  numComponents: Int) extends PointTransformer {

  def apply(points: IndexedSeq[Point]) = {
    val scaledPoints = scaler(points)
    val pointMatrix = new Matrix(scaledPoints.map(_.coord.toArray).toArray)
    val transformed = pca.transform(pointMatrix, PCA.TransformationType.ROTATION)
    transformed.getArray.map { transformedCoord =>
      Point(transformedCoord.take(numComponents).toIndexedSeq)
    }
  }

}

object PcaTransformer {
  def apply(points: IndexedSeq[Point]) = {
    
    // First scale the points.
    val scaler = ZscoreTransformer(points)
    val scaledPoints = scaler(points)
    
    // Compute the PCA from the scaled points.
    val pca = new PCA(new Matrix(scaledPoints.map(_.coord.toArray).toArray))

    // Figure out how many components are needed to explain 95% of the variance.
    val eigVals = (0 until pca.getOutputDimsNo).map(pca.getEigenvalue(_))
    val eigValsSq = eigVals.map(x => x * x)
    val propVariance = eigValsSq.map(_ / eigValsSq.sum)
    val numComponents = propVariance.scan(0.0)(_ + _).indexWhere(.95<)

    // Create the PCA Transformer
    new PcaTransformer(pca, scaler, numComponents)
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
