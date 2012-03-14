package opennlp.scalabha.tag.support

import org.junit.Assert._
import org.junit._
import opennlp.scalabha.util.Probability
import opennlp.scalabha.util.Probability._
import opennlp.scalabha.util.Pattern.{ -> }
import opennlp.scalabha.util.CollectionUtils._
import org.apache.log4j.Logger
import org.apache.log4j.Level

class CondCountsTransformerTests {

  @Test
  def test_PassthroughCondCountsTransformer_DefaultCounts_double() {
    val transformer = new PassthroughCondCountsTransformer[Char, Symbol]()

    val counts = Map(
      'A' -> Map('a -> 1., 'b -> 4.),
      'B' -> Map('a -> 5., 'b -> 3.),
      'C' -> Map('a -> 2.))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(1. / 5.), d('A')('a))
    assertEqualsProb(Probability(4. / 5.), d('A')('b))
    assertEqualsProb(Probability.zero, d('A')('def))
    assertEqualsProb(Probability(5. / 8.), d('B')('a))
    assertEqualsProb(Probability(3. / 8.), d('B')('b))
    assertEqualsProb(Probability.zero, d('B')('def))
    assertEqualsProb(Probability.one, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_Map_int() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        delegate = MockCondCountsTransformer(
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 27.), 0., 0.),
            'C' -> DefaultedFreqCounts(Map('b -> 29.), 0., 0.))),
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7.), 3., 2.),
            'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9.), 4., 3.),
            'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  0  |  0  |
     *   b |  4  |  3  |  0  |  0  |
     *   c |  0  |  0  |  0  |  0  |
     *   d |  2  |  0  |  0  |  0  |
     * def |  0  |  0  |  0  |  0  |
     *   ==+=====+=====+=====+=====+
     *     | 11  |  9  |  0  |  0  |
     *     |+ 0  |  0  |  0  |  0  |
     * tot | 11  |  9  |  0  |  0  |
     * 
     */

    val counts = Map(
      'A' -> Map('a -> 27),
      'C' -> Map('b -> 29))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(5. / 11.), d('A')('a))
    assertEqualsProb(Probability(4. / 11.), d('A')('b))
    assertEqualsProb(Probability(0. / 11.), d('A')('c))
    assertEqualsProb(Probability(2. / 11.), d('A')('d))
    assertEqualsProb(Probability(0. / 11.), d('A')('def))
    assertEqualsProb(Probability(6. / 9.), d('B')('a))
    assertEqualsProb(Probability(3. / 9.), d('B')('b))
    assertEqualsProb(Probability(0. / 9.), d('B')('c))
    assertEqualsProb(Probability(0. / 9.), d('B')('d))
    assertEqualsProb(Probability(0. / 9.), d('B')('def))
    assertEqualsProb(Probability.zero, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('c))
    assertEqualsProb(Probability.zero, d('C')('d))
    assertEqualsProb(Probability.zero, d('C')('def))
    assertEqualsProb(Probability.zero, d('D')('a))
    assertEqualsProb(Probability.zero, d('D')('b))
    assertEqualsProb(Probability.zero, d('D')('c))
    assertEqualsProb(Probability.zero, d('D')('d))
    assertEqualsProb(Probability.zero, d('D')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        delegate = MockCondCountsTransformer(
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
            'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7.), 3., 2.),
            'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9.), 4., 3.),
            'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  0  |  0  |
     *   b |  4  |  3  |  0  |  0  |
     *   c |  0  |  0  |  0  |  0  |
     *   d |  2  |  0  |  0  |  0  |
     * def |  0  |  0  |  0  |  0  |
     *   ==+=====+=====+=====+=====+
     *     | 11  |  9  |  0  |  0  |
     *     |+ 0  |  0  |  0  |  0  |
     * tot | 11  |  9  |  0  |  0  |
     * 
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(5. / 11.), d('A')('a))
    assertEqualsProb(Probability(4. / 11.), d('A')('b))
    assertEqualsProb(Probability(0. / 11.), d('A')('c))
    assertEqualsProb(Probability(2. / 11.), d('A')('d))
    assertEqualsProb(Probability(0. / 11.), d('A')('def))
    assertEqualsProb(Probability(6. / 9.), d('B')('a))
    assertEqualsProb(Probability(3. / 9.), d('B')('b))
    assertEqualsProb(Probability(0. / 9.), d('B')('c))
    assertEqualsProb(Probability(0. / 9.), d('B')('d))
    assertEqualsProb(Probability(0. / 9.), d('B')('def))
    assertEqualsProb(Probability.zero, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('c))
    assertEqualsProb(Probability.zero, d('C')('d))
    assertEqualsProb(Probability.zero, d('C')('def))
    assertEqualsProb(Probability.zero, d('D')('a))
    assertEqualsProb(Probability.zero, d('D')('b))
    assertEqualsProb(Probability.zero, d('D')('c))
    assertEqualsProb(Probability.zero, d('D')('d))
    assertEqualsProb(Probability.zero, d('D')('def))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1,
        delegate = MockCondCountsTransformer(
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
            'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7.), 3., 2.),
            'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9.), 4., 3.),
            'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |
     *   ==+=======+=======+=======+=======+
     *   a |  5.1  |  6.1  |  8.1  |   -   |
     *   b |  4.1  |  3.1  |  1.1  |   -   |
     *   c |  7.1  |  9.1  |  1.1  |   -   |
     *   d |   -   |   -   |   -   |   -   |
     * def |  2.1  |  3.1  |  1.1  |   -   |
     *   ==+=======+=======+=======+=======+
     *     |  16.3 |  18.3 | 10.3  |   -   |
     *     | + 3.1 |   4.1 |  5.1  |   -   |
     * tot |  19.4 |  22.4 | 15.4  |   -   |
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability(5.1 / 19.4), d('A')('a))
    assertEqualsProb(Probability(4.1 / 19.4), d('A')('b))
    assertEqualsProb(Probability(7.1 / 19.4), d('A')('c))
    assertEqualsProb(Probability(2.1 / 19.4), d('A')('d))
    assertEqualsProb(Probability(2.1 / 19.4), d('A')('def))
    assertEqualsProb(Probability(6.1 / 22.4), d('B')('a))
    assertEqualsProb(Probability(3.1 / 22.4), d('B')('b))
    assertEqualsProb(Probability(9.1 / 22.4), d('B')('c))
    assertEqualsProb(Probability(3.1 / 22.4), d('B')('d))
    assertEqualsProb(Probability(3.1 / 22.4), d('B')('def))
    assertEqualsProb(Probability(8.1 / 15.4), d('C')('a))
    assertEqualsProb(Probability(1.1 / 15.4), d('C')('b))
    assertEqualsProb(Probability(1.1 / 15.4), d('C')('c))
    assertEqualsProb(Probability(1.1 / 15.4), d('C')('d))
    assertEqualsProb(Probability(1.1 / 15.4), d('C')('def))
  }

  @Test
  def test_ConstrainingCondCountsTransformer_before_AddLambdaSmoothingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1,
        new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
          delegate = MockCondCountsTransformer(
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
              'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7.), 3., 2.),
              'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9.), 4., 3.),
              'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.))))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After constraining:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  0  |  0  |
     *   b |  4  |  3  |  0  |  0  |
     *   c |  0  |  0  |  0  |  0  |
     *   d |  2  |  0  |  0  |  0  |
     * def |  0  |  0  |  0  |  0  |
     *   ==+=====+=====+=====+=====+
     *     | 11  |  9  |  0  |  0  |
     *     |+ 0  |  0  |  0  |  0  |
     * tot | 11  |  9  |  0  |  0  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |
     *   ==+=======+=======+=======+=======+
     *   a |  5.1  |  6.1  |  0.1  |  0.1  |
     *   b |  4.1  |  3.1  |  0.1  |  0.1  |
     *   c |  0.1  |  0.1  |  0.1  |  0.1  |
     *   d |  2.1  |  0.1  |  0.1  |  0.1  |
     * def |  0.1  |  0.1  |  0.1  |  0.1  |
     *   ==+=======+=======+=======+=======+
     *     |  11.4 |  9.4  |  0.4  |  0.4  |
     *     | + 0.1 |  0.1  |  0.1  |  0.1  |
     * tot |  11.5 |  9.5  |  0.5  |  0.5  |
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)
    assertEqualsProb(Probability(5.1 / 11.5), d('A')('a))
    assertEqualsProb(Probability(4.1 / 11.5), d('A')('b))
    assertEqualsProb(Probability(0.1 / 11.5), d('A')('c))
    assertEqualsProb(Probability(2.1 / 11.5), d('A')('d))
    assertEqualsProb(Probability(0.1 / 11.5), d('A')('def))
    assertEqualsProb(Probability(6.1 / 9.5), d('B')('a))
    assertEqualsProb(Probability(3.1 / 9.5), d('B')('b))
    assertEqualsProb(Probability(0.1 / 9.5), d('B')('c))
    assertEqualsProb(Probability(0.1 / 9.5), d('B')('d))
    assertEqualsProb(Probability(0.1 / 9.5), d('B')('def))
    assertEqualsProb(Probability(0.1 / 0.5), d('C')('a))
    assertEqualsProb(Probability(0.1 / 0.5), d('C')('b))
    assertEqualsProb(Probability(0.1 / 0.5), d('C')('c))
    assertEqualsProb(Probability(0.1 / 0.5), d('C')('d))
    assertEqualsProb(Probability(0.1 / 0.5), d('C')('def))
    assertEqualsProb(Probability(0.1 / 0.5), d('D')('a))
    assertEqualsProb(Probability(0.1 / 0.5), d('D')('b))
    assertEqualsProb(Probability(0.1 / 0.5), d('D')('c))
    assertEqualsProb(Probability(0.1 / 0.5), d('D')('d))
    assertEqualsProb(Probability(0.1 / 0.5), d('D')('def))
  }

  @Test
  def test_AddLambdaSmoothingCondCountsTransformer_before_ConstrainingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new ConstrainingCondCountsTransformer[Char, Symbol](validEntries = Map('A' -> Set('a, 'b, 'd), 'B' -> Set('a, 'b), 'D' -> Set('d)),
        new AddLambdaSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.1,
          delegate = MockCondCountsTransformer(
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
              'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
            new DefaultedCondFreqCounts(Map(
              'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7.), 3., 2.),
              'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9.), 4., 3.),
              'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.))))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   d |  -  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 16  | 15  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 19  | 19  | 13  |  -  |
     * 
     * After smoothing:
     *  
     *     |   A   |   B   |   C   |   D   |
     *   ==+=======+=======+=======+=======+
     *   a |  5.1  |  6.1  |  8.1  |   -   |
     *   b |  4.1  |  3.1  |  1.1  |   -   |
     *   c |  7.1  |  9.1  |  1.1  |   -   |
     *   d |   -   |   -   |   -   |   -   |
     * def |  2.1  |  3.1  |  1.1  |   -   |
     *   ==+=======+=======+=======+=======+
     *     |  16.3 |  18.3 | 10.3  |   -   |
     *     | + 3.1 |   4.1 |  5.1  |   -   |
     * tot |  19.4 |  22.4 | 15.4  |   -   |
     * 
     * After constraining:
     *  
     *     |   A   |   B   |   C   |   D   |
     *   ==+=======+=======+=======+=======+
     *   a |  5.1  |  6.1  |   0   |   0   |
     *   b |  4.1  |  3.1  |   0   |   0   |
     *   c |   0   |   0   |   0   |   0   |
     *   d |  2.1  |   0   |   0   |   0   |
     * def |   0   |   0   |   0   |   0   |
     *   ==+=======+=======+=======+=======+
     *     |  11.3 |  9.2  |   0   |   0   |
     *     | + 0   |   0   |   0   |   0   |
     * tot |  11.3 |  9.2  |   0   |   0   |
     * 
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability(5.1 / 11.3), d('A')('a))
    assertEqualsProb(Probability(4.1 / 11.3), d('A')('b))
    assertEqualsProb(Probability(0.0 / 11.3), d('A')('c))
    assertEqualsProb(Probability(2.1 / 11.3), d('A')('d))
    assertEqualsProb(Probability(0.0 / 11.3), d('A')('def))
    assertEqualsProb(Probability(6.1 / 9.2), d('B')('a))
    assertEqualsProb(Probability(3.1 / 9.2), d('B')('b))
    assertEqualsProb(Probability(0.0 / 9.2), d('B')('c))
    assertEqualsProb(Probability(0.0 / 9.2), d('B')('d))
    assertEqualsProb(Probability(0.0 / 9.2), d('B')('def))
    assertEqualsProb(Probability.zero, d('C')('a))
    assertEqualsProb(Probability.zero, d('C')('b))
    assertEqualsProb(Probability.zero, d('C')('c))
    assertEqualsProb(Probability.zero, d('C')('d))
    assertEqualsProb(Probability.zero, d('C')('def))
    assertEqualsProb(Probability.zero, d('D')('a))
    assertEqualsProb(Probability.zero, d('D')('b))
    assertEqualsProb(Probability.zero, d('D')('c))
    assertEqualsProb(Probability.zero, d('D')('d))
    assertEqualsProb(Probability.zero, d('D')('def))
  }

  @Test
  def test_EisnerSmoothingCondCountsTransformer_DefaultCounts_double() {
    val transformer =
      new EisnerSmoothingCondCountsTransformer[Char, Symbol](lambda = 0.2, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 0.1),
        delegate = MockCondCountsTransformer(
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
            'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
          new DefaultedCondFreqCounts(Map(
            'A' -> DefaultedFreqCounts(Map('a -> 5., 'b -> 4., 'c -> 7., 'x -> 1, 'y -> 1, 'z -> 1), 3., 2.),
            'B' -> DefaultedFreqCounts(Map('a -> 6., 'c -> 9., 'x -> 1, 'y -> 1), 4., 3.),
            'C' -> DefaultedFreqCounts(Map('a -> 8.), 5., 1.)))))

    /*
     * Starting counts:
     *  
     *     |  A  |  B  |  C  |  D  |
     *   ==+=====+=====+=====+=====+
     *   a |  5  |  6  |  8  |  -  |
     *   b |  4  |  -  |  -  |  -  |
     *   c |  7  |  9  |  -  |  -  |
     *   x |  1  |  1  |  -  |  -  |
     *   y |  1  |  1  |  -  |  -  |
     *   z |  1  |  -  |  -  |  -  |
     * def |  2  |  3  |  1  |  -  |
     *   ==+=====+=====+=====+=====+
     * uni |  3  |  2  |  0  |  -  |
     *   ==+=====+=====+=====+=====+
     *     | 19  | 17  |  8  |  -  |
     *     |+ 3  |  4  |  5  |  -  |
     * tot | 22  | 21  | 13  |  -  |
     * 
     * Compute backoff:
     * 
     *       counts | smooth |  prob  |
     *  ===+========+========+========+
     *   a |   19   |  19.1  |        |
     *   b |    4   |   4.1  |        |
     *   c |   16   |  16.1  |        |
     *   x |    2   |   2.1  |        |
     *   y |    2   |   2.1  |        |
     *   z |    1   |   1.1  |        |
     * def |    -   |    .1  |        |
     *  ===+========+========+========+
     *     |   44   |  44.6  |        |
     *     |        | +  .1  |        |
     *     |   44   |  44.7  |        |
     * 
     * 
     * After smoothing:
     *  
     *     |             A            |            B             |             C            |
     *   ==+==========================+==========================+==========================+
     *   a | 5 + .2 * 3 * (19.1/44.7) | 6 + .2 * 2 * (19.1/44.7) | 8 + .2 * 0 * (19.1/44.7) |
     *   b | 4 + .2 * 3 * ( 4.1/44.7) | 3 + .2 * 2 * ( 4.1/44.7) | 1 + .2 * 0 * ( 4.1/44.7) |
     *   c | 7 + .2 * 3 * (16.1/44.7) | 9 + .2 * 2 * (16.1/44.7) | 1 + .2 * 0 * (16.1/44.7) |
     *   x | 1 + .2 * 3 * ( 2.1/44.7) | 1 + .2 * 2 * ( 2.1/44.7) | 1 + .2 * 0 * ( 2.1/44.7) |
     *   y | 1 + .2 * 3 * ( 2.1/44.7) | 1 + .2 * 2 * ( 2.1/44.7) | 1 + .2 * 0 * ( 2.1/44.7) |
     *   z | 1 + .2 * 3 * ( 1.1/44.7) | 3 + .2 * 2 * ( 1.1/44.7) | 1 + .2 * 0 * ( 1.1/44.7) |
     * def | 2 + .2 * 3 * (  .1/44.7) | 3 + .2 * 2 * (  .1/44.7) | 1 + .2 * 0 * (  .1/44.7) |
     *   ==+==========================+==========================+==========================+
     *     |+3 + .2 * 3 * (  .1/44.7) |+4 + .2 * 2 * (  .1/44.7) |+5 + .2 * 0 * (  .1/44.7) |
     *     | 22 + .2 * 3 * ( 1 )      | 27 + .2 * 2 * ( 1 )      | 18 + .2 * 0 * ( 1 )      |
     *  
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability((5 + .2 * 3 * (19.1/44.7)) / (22 + .2 * 3)), d('A')('a))
    assertEqualsProb(Probability((4 + .2 * 3 * ( 4.1/44.7)) / (22 + .2 * 3)), d('A')('b))
    assertEqualsProb(Probability((7 + .2 * 3 * (16.1/44.7)) / (22 + .2 * 3)), d('A')('c))
    assertEqualsProb(Probability((1 + .2 * 3 * ( 2.1/44.7)) / (22 + .2 * 3)), d('A')('x))
    assertEqualsProb(Probability((1 + .2 * 3 * ( 2.1/44.7)) / (22 + .2 * 3)), d('A')('y))
    assertEqualsProb(Probability((1 + .2 * 3 * ( 1.1/44.7)) / (22 + .2 * 3)), d('A')('z))
    assertEqualsProb(Probability((2 + .2 * 3 * (  .1/44.7)) / (22 + .2 * 3)), d('A')('def))

    assertEqualsProb(Probability((6 + .2 * 2 * (19.1/44.7)) / (27 + .2 * 2)), d('B')('a))
    assertEqualsProb(Probability((3 + .2 * 2 * ( 4.1/44.7)) / (27 + .2 * 2)), d('B')('b))
    assertEqualsProb(Probability((9 + .2 * 2 * (16.1/44.7)) / (27 + .2 * 2)), d('B')('c))
    assertEqualsProb(Probability((1 + .2 * 2 * ( 2.1/44.7)) / (27 + .2 * 2)), d('B')('x))
    assertEqualsProb(Probability((1 + .2 * 2 * ( 2.1/44.7)) / (27 + .2 * 2)), d('B')('y))
    assertEqualsProb(Probability((3 + .2 * 2 * ( 1.1/44.7)) / (27 + .2 * 2)), d('B')('z))
    assertEqualsProb(Probability((3 + .2 * 2 * (  .1/44.7)) / (27 + .2 * 2)), d('B')('def))

    assertEqualsProb(Probability((8 + .2 * 0 * (19.1/44.7)) / (18 + .2 * 0)), d('C')('a))
    assertEqualsProb(Probability((1 + .2 * 0 * ( 4.1/44.7)) / (18 + .2 * 0)), d('C')('b))
    assertEqualsProb(Probability((1 + .2 * 0 * (16.1/44.7)) / (18 + .2 * 0)), d('C')('c))
    assertEqualsProb(Probability((1 + .2 * 0 * ( 2.1/44.7)) / (18 + .2 * 0)), d('C')('x))
    assertEqualsProb(Probability((1 + .2 * 0 * ( 2.1/44.7)) / (18 + .2 * 0)), d('C')('y))
    assertEqualsProb(Probability((1 + .2 * 0 * ( 1.1/44.7)) / (18 + .2 * 0)), d('C')('z))
    assertEqualsProb(Probability((1 + .2 * 0 * (  .1/44.7)) / (18 + .2 * 0)), d('C')('def))
  }

  @Test
  def test_RandomCondCountsTransformer_DefaultCounts_double() {
    val transformer = new RandomCondCountsTransformer[Char, Symbol](
      maxCount = 10,
      delegate = MockCondCountsTransformer(
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
          'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26.))),
        new DefaultedCondFreqCounts(Map(
          'A' -> DefaultedFreqCounts(Map('a -> 1., 'b -> 2.), .1, .2),
          'B' -> DefaultedFreqCounts(Map('a -> 3.), .3, .4)))))

    /* As it happens, the first random values for this test are:
     * 0 8 9 7 5 3
     *
     * without the random weighting, the frequency distribution is:
     *
     *     |  A  |  B  |
     *   ==+=====+=====+
     *   a |  1  |  2  |
     *   b |  3  |  -  |
     * def | 0.2 | 0.4 |
     *   ==+=====+=====+
     *     |  4  |  2  |
     *     |+ .1 |  .3 |
     *     | 4.1 | 2.7 |
     *
     * Given the order the frequencies are (re-)calculated, this is randomly
     * adjusted to:
     *
     *   |    A    |    B    |
     * ==+=========+=========+
     * a |  1 + 0  |  2 + 9  |
     * b |  3 + 5  | 0.4 + 5 |
     * 
     * or
     * 
     *     |   A   |   B   |
     *   ==+=======+=======+
     *   a |   1   |  11   |
     *   b |   8   |  5.4  |
     * def |  0.2  |  0.4  |
     *   ==+=======+=======+
     *     |   9   | 16.4  |
     *     |+  .1  |   .3  |
     *     |  9.1  | 16.7  |
     * 
     */

    val counts = new DefaultedCondFreqCounts(Map(
      'A' -> DefaultedFreqCounts(Map('a -> 27.), 21., 22.),
      'C' -> DefaultedFreqCounts(Map('b -> 29.), 25., 26)))

    val r = transformer(counts)
    // TODO: assert counts 

    val d = CondFreqDist(r)

    assertEqualsProb(Probability(1.0 / 9.1), d('A')('a))
    assertEqualsProb(Probability(8.0 / 9.1), d('A')('b))
    assertEqualsProb(Probability(0.2 / 9.1), d('A')('def))
    assertEqualsProb(Probability(11. / 16.7), d('B')('a))
    assertEqualsProb(Probability(5.4 / 16.7), d('B')('b))
    assertEqualsProb(Probability(0.4 / 16.7), d('B')('def))
  }

  case class MockCondCountsTransformer[A, B](expected: DefaultedCondFreqCounts[A, B, Double], returned: DefaultedCondFreqCounts[A, B, Double]) extends CondCountsTransformer[A, B] {
    override def apply(counts: DefaultedCondFreqCounts[A, B, Double]) = {
      for ((eA -> e, cA -> c) <- (expected.counts zipEqual counts.counts)) {
        assertEquals(eA, cA)
        val DefaultedFreqCounts(eC, eT, eD) = e
        val DefaultedFreqCounts(cC, cT, cD) = c
        assertEquals(eC, cC)
        assertEqualsDouble(eT, cT)
        assertEqualsDouble(eD, cD)
      }
      returned
    }
  }

  def assertEqualsProb(a: Probability, b: Probability) {
    assertEquals(a.toDouble, b.toDouble, 0.001)
  }

  def assertEqualsDouble(a: Double, b: Double) {
    assertEquals(a.toDouble, b.toDouble, 0.0000001)
  }

}

object CondCountsTransformerTests {

  @BeforeClass def turnOffLogging() {
    Logger.getRootLogger.setLevel(Level.OFF)
  }

}
