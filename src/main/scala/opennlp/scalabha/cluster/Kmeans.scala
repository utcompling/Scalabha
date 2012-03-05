package opennlp.scalabha.cluster

import org.apache.commons.logging.LogFactory
import org.apache.log4j.Logger
import org.apache.log4j.Level

import opennlp.scalabha.util.CollectionUtils._

class Kmeans(
  points: IndexedSeq[Point],
  distance: DistanceFunction,
  minChangeInDispersion: Double = 0.0001,
  maxIterations: Int = 10) {

  private val LOG = LogFactory.getLog(Kmeans.getClass)

  private[this] val numDimensions = points.head.numDimensions
  private[this] val origin = Point(IndexedSeq.fill(numDimensions)(0.0))

  // Actually, this should be "truly" random, but it is seeded with 13 to
  // ensure consistency for homework. See the commented out line for a seed
  // based on the current time.
  private[this] val random = new util.Random(13)
  //private[this] val random = new util.Random(compat.Platform.currentTime)

  // Run the k-means algorithm on this set of points for some given k.
  def run(k: Int, restarts: Int = 25) = {
    val (dispersions, centroidGroups) = (1 to restarts).map { _ =>
      moveCentroids(chooseRandomCentroids(k))
    }.unzip

    val bestDispersion = dispersions.min
    val bestIndex = dispersions.indexWhere(bestDispersion==)
    val bestCentroids = centroidGroups(bestIndex)
    LOG.debug("Dispersion: " + bestDispersion)
    LOG.debug("Centroids: ")
    if (LOG.isDebugEnabled)
      bestCentroids.foreach(println)

    (bestDispersion, bestCentroids)
  }

  // Run from a given starting set of centroids.
  def moveCentroids(centroids: IndexedSeq[Point]) = {

    // Inner recursive function for computing next centroids
    def inner(centroids: IndexedSeq[Point],
      lastDispersion: Double,
      iteration: Int): (Double, IndexedSeq[Point]) = {
      LOG.debug("Iteration " + iteration)

      val (dispersion, memberships) = computeClusterMemberships(centroids)
      val updatedCentroids = computeCentroids(memberships)
     
      LOG.debug("Dispersion: " + dispersion)
      LOG.debug("Centroids: ")
      if (LOG.isDebugEnabled)
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
      .groupByKey
      .mapValues { groupForCentroid =>
        val pointsForCentroid = groupForCentroid
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



