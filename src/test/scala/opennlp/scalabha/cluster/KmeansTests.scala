package opennlp.scalabha.cluster

import org.junit.Test
import org.junit.Assert._
import org.junit.BeforeClass
import org.apache.log4j.Logger
import org.apache.log4j.Level

class KmeansTests {

  @Test
  def test_kmeans() {
    val clusters =
      Vector(
        Vector(Point(Vector(0, 0)), Point(Vector(0, 1)), Point(Vector(1, 0)), Point(Vector(1, 1))),
        Vector(Point(Vector(8, 8)), Point(Vector(8, 9)), Point(Vector(9, 8)), Point(Vector(9, 9))),
        Vector(Point(Vector(0, 8)), Point(Vector(0, 9)), Point(Vector(1, 8)), Point(Vector(1, 9))))
    val points: Vector[Point] = clusters.flatten

    val kmeans = new Kmeans(
      points = points,
      distance = EuclideanDistance,
      minChangeInDispersion = 0.0001,
      maxIterations = 100)

    val (bestDispersion, bestCentroids) = kmeans.run(k = 3, restarts = 5)

    assertEquals(6.0, bestDispersion, 0.00000001)
    val IndexedSeq(a, b, c) = bestCentroids
    assertEquals(Point(Vector(8.5, 8.5)), a)
    assertEquals(Point(Vector(0.5, 8.5)), b)
    assertEquals(Point(Vector(0.5, 0.5)), c)
  }

}

object KmeansTests {
  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }
}
