package opennlp.scalabha.cluster

class Kmeans(
  points: IndexedSeq[Point],
  distance: DistanceFunction,
  minChangeInDispersion: Double = 0.0001,
  maxIterations: Int = 100) {

  private[this] val numDimensions = points.head.numDimensions
  private[this] val origin = Point(IndexedSeq.fill(numDimensions)(0.0))

  private[this] val random = new util.Random(13)

  // Run the k-means algorithm on this set of points for some given k.
  def run(k: Int, restarts: Int = 25) = {
    val (dispersions, centroidGroups) = (1 to restarts).map { _ =>
      moveCentroids(chooseRandomCentroids(k))
    }.unzip

    val bestDispersion = dispersions.min
    val bestIndex = dispersions.indexWhere(bestDispersion==)
    (dispersions(bestIndex), centroidGroups(bestIndex))
  }

  // Run from a given starting set of centroids.
  def moveCentroids(centroids: IndexedSeq[Point]) = {
    
    // Inner recursive function for computing next centroids
    def inner(centroids: IndexedSeq[Point],
      lastDispersion: Double,
      iteration: Int): (Double, IndexedSeq[Point]) = {
      println("Iteration " + iteration)

      val (dispersion, memberships) = computeClusterMemberships(centroids)
      val updatedCentroids = computeCentroids(memberships)

      println("Dispersion: " + dispersion)
      updatedCentroids.foreach(println)

      val dispersionChange = lastDispersion - dispersion

      if (iteration > maxIterations || dispersionChange < minChangeInDispersion)
        (lastDispersion, centroids)
      else
        inner(updatedCentroids, dispersion, iteration + 1)
    }
    
    inner(centroids, Double.PositiveInfinity, 1)
  }
  // Given a sequence of centroids, compute the cluster memberships for 
  // each point.
  def computeClusterMemberships(centroids: IndexedSeq[Point]) = {
    val (squaredDistances, memberships) = points.map { point =>
      val distances = centroids.map(distance(_, point))
      val shortestDistance = distances.min
      val closestCentroid = distances.indexWhere(shortestDistance==)
      (shortestDistance * shortestDistance, closestCentroid)
    }.unzip
    (squaredDistances.sum, memberships)
  }

  // Given memberships for each point, compute the centroid for each
  // cluster.
  private[this] def computeCentroids(memberships: IndexedSeq[Int]) = {
    memberships.zip(points)
      .groupBy(_._1)
      .mapValues { groupForCentroid =>
        val (_, pointsForCentroid) = groupForCentroid.unzip
        val summed = pointsForCentroid.foldLeft(origin)(_ ++ _)
        summed / pointsForCentroid.length.toDouble
      }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .toIndexedSeq
  }

  private[this] def chooseRandomCentroids(k: Int) = {
    val randomIndices = random.shuffle(points.indices.toList).take(k)
    randomIndices.map(points(_)).toIndexedSeq
  }

}

object Kmeans {

  def main(args: Array[String]) {

    val distanceOption = "euclidean"
    val maxIterations = 100
    val minChangeInDispersion = .0001

    val input = io.Source.fromFile(args(0)).getLines.map(lineToInfo)
    val (ids, labels, coords) = input.toList.unzip3

    val points: IndexedSeq[Point] = coords.map(coord => Point(coord)).toIndexedSeq

    val distance = distanceOption match {
      case "cosine" => CosineDistance
      case "manhattan" => ManhattanDistance
      case "euclidean" => EuclideanDistance
      case _ =>
        throw new MatchError("Invalid distance function: " + distanceOption)
    }

    val kmeans = new Kmeans(points, distance, minChangeInDispersion, maxIterations)

    val (dispersion, centroids) = kmeans.run(3)
//    val results = (1 to 10).map(kmeans.run(_))
//    val (dispersions, centroidsPerK) = results.unzip
//    dispersions.zipWithIndex.foreach {
//      case (dispersion, index) => println(index + 1 + ": " + dispersion)
//    }
  }

  private def lineToInfo(inputLine: String): (String, String, IndexedSeq[Double]) = {
    val id :: label :: coord = inputLine.split(" ").toList
    (id, label, coord.map(_.toDouble).toIndexedSeq)
  }

}

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

abstract class DistanceFunction extends ((Point, Point) => Double)

object CosineDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = 1 - x.dotProduct(y) / (x.norm * y.norm)
}

object ManhattanDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = (x - y).abs.sum
}

object EuclideanDistance extends DistanceFunction {
  def apply(x: Point, y: Point) = (x - y).norm
}



