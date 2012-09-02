package opennlp.scalabha.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.IterableLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.util.Random

object CollectionUtils {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  class Enriched_toTuple_Seq[A](elements: Seq[A]) {
    def toTuple2 = elements match { case Seq(a, b) => (a, b) }
    def toTuple3 = elements match { case Seq(a, b, c) => (a, b, c) }
    def toTuple4 = elements match { case Seq(a, b, c, d) => (a, b, c, d) }
    def toTuple5 = elements match { case Seq(a, b, c, d, e) => (a, b, c, d, e) }
  }
  implicit def enrich_toTuple_Seq[A](elements: Seq[A]): Enriched_toTuple_Seq[A] =
    new Enriched_toTuple_Seq(elements)

  //////////////////////////////////////////////////////
  // countCompare(p: A => Boolean, count: Int): Int
  //   - Compares the number of items satisfying a predicate to a test value.
  //   - Functionally equivalent to (but more efficient than):
  //         this.count(p).compareTo(count)
  //////////////////////////////////////////////////////

  class Enriched_countCompare_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param p       the predicate used to test elements.
     *   @param count   the test value that gets compared with the count.
     *   @return A value `x` where
     *   {{{
     *        x <  0       if this.count(p) <  count
     *        x == 0       if this.count(p) == count
     *        x >  0       if this.count(p) >  count
     *   }}}
     *  The method as implemented here does not call `length` directly; its running time
     *  is `O(length min count)` instead of `O(length)`.
     */
    def countCompare(p: A => Boolean, count: Int): Int = {
      val itr = self.toIterator
      var i = 0
      while (itr.hasNext && i <= count) {
        if (p(itr.next))
          i += 1
      }
      i - count
    }
  }
  implicit def enrich_countCompare_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enriched_countCompare_GenTraversableOnce[A] =
    new Enriched_countCompare_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  class Enriched_split_Iterator[A](self: Iterator[A]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A])

    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      new Iterator[That] {
        def next(): That = {
          if (!self.hasNext) throw new RuntimeException("next on empty iterator")

          val b = builder
          while (self.hasNext) {
            val x = self.next
            if (x == delim)
              return b.result
            else
              b += x
          }
          return b.result
        }

        def hasNext() = self.hasNext
      }
  }
  implicit def enrich_split_Iterator[A](self: Iterator[A]): Enriched_split_Iterator[A] =
    new Enriched_split_Iterator(self)

  class Enriched_split_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]))
  }
  implicit def enrich_split_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_split_GenTraversableLike[A, Repr] =
    new Enriched_split_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // splitAt(n: Int) 
  //   - Split this collection at the specified index 
  //   - Extend Traversable.splitAt to Iterator
  //////////////////////////////////////////////////////

  class Enriched_splitAt_Iterator[A](self: Iterator[A]) {
    /**
     * Safely split this iterator at the specified index.  The 'first'
     * iterator must be exhausted completely before the items in the 'second'
     * iterator can be accessed.
     *
     * Inspired by Traversable.splitAt
     *
     * @param n	The index at which to split the collection
     * @return	a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      var i = 0

      val first: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(hasNext, "first has already been read completely")
            i += 1; self.next
          }
          def hasNext() = i < n && self.hasNext
        }

      val second: Iterator[A] =
        new Iterator[A] {
          def next(): A = {
            assert(i >= n, "first has NOT YET been read completely")
            assert(hasNext, "second has already been read completely")
            i += 1; self.next
          }
          def hasNext() = self.hasNext
        }

      (first, second)
    }
  }
  implicit def enrich_splitAt_Iterator[A](self: Iterator[A]): Enriched_splitAt_Iterator[A] =
    new Enriched_splitAt_Iterator(self)

  //////////////////////////////////////////////////////
  // sumBy[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  class Enriched_sumBy_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f	A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      val itr = self.toIterator
      var accum = num.zero
      while (itr.hasNext)
        accum = num.plus(accum, f(itr.next))
      return accum
    }
  }
  implicit def enrich_sumBy_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enriched_sumBy_GenTraversableOnce[A] =
    new Enriched_sumBy_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[(A,B)]
  //   - Map a function over the collection, returning a set of pairs consisting 
  //     of the original item and the result of the function application
  //   - Functionally equivalent to:
  //         map(x => x -> f(x))
  //////////////////////////////////////////////////////

  class Enriched_mapTo_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return the new collection
     */
    def mapTo[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += x -> f(x)
      b.result
    }
  }
  implicit def enrich_mapTo_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_mapTo_GenTraversableLike[A, Repr] =
    new Enriched_mapTo_GenTraversableLike(self)

  class Enriched_mapTo_Iterator[A](self: Iterator[A]) {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return a new iterator
     */
    def mapTo[B](f: A => B): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = self.hasNext
      def next() = {
        val x = self.next
        x -> f(x)
      }
    }
  }
  implicit def enrich_mapTo_Iterator[A](self: Iterator[A]): Enriched_mapTo_Iterator[A] =
    new Enriched_mapTo_Iterator(self)

  //////////////////////////////////////////////////////
  // mapToVal[B](v: B): Repr[(A,B)]
  //   - Map each item in the collection to a particular value
  //   - Functionally equivalent to:
  //         map(x => x -> v)
  //////////////////////////////////////////////////////

  class Enriched_mapToVal_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return the new collection
     */
    def mapToVal[B, That](v: => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (x <- self) b += x -> v
      b.result
    }
  }
  implicit def enrich_mapToVal_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_mapToVal_GenTraversableLike[A, Repr] =
    new Enriched_mapToVal_GenTraversableLike(self)

  class Enriched_mapToVal_Iterator[A](self: Iterator[A]) {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return a new iterator
     */
    def mapToVal[B](v: => B): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = self.hasNext
      def next() = self.next -> v
    }
  }
  implicit def enrich_mapToVal_Iterator[A](self: Iterator[A]): Enriched_mapToVal_Iterator[A] =
    new Enriched_mapToVal_Iterator(self)

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  class Enriched_zipSafe_Iterator[A](self: Iterator[A]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[B](that: GenTraversableOnce[B]) = {
      val thatItr = that.toIterator
      new Iterator[(A, B)] {
        def hasNext = {
          val hn = self.hasNext
          assert(hn == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          hn
        }
        def next() = {
          assert(self.hasNext == thatItr.hasNext, "Attempting to zipSafe collections of different lengths.")
          (self.next, thatItr.next)
        }
      }
    }
  }
  implicit def enrich_zipSafe_Iterator[A](self: Iterator[A]): Enriched_zipSafe_Iterator[A] =
    new Enriched_zipSafe_Iterator(self)

  class Enriched_zipSafe_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }
  implicit def enrich_zipSafe_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_zipSafe_GenTraversableLike[A, Repr] =
    new Enriched_zipSafe_GenTraversableLike(self)

  class Enriched_zipSafe_Tuple_of_Iterator[A, B](self: (Iterator[A], GenTraversableOnce[B])) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }
  implicit def enrich_zipSafe_Tuple_of_Iterator[A, B](self: (Iterator[A], GenTraversableOnce[B])): Enriched_zipSafe_Tuple_of_Iterator[A, B] =
    new Enriched_zipSafe_Tuple_of_Iterator(self)

  class Enriched_zipSafe_Tuple_of_GenTraversableLike[A, Repr <: GenTraversable[A], B](self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self._1.toIterator zipSafe self._2)
      b.result
    }
  }
  implicit def enrich_zipSafe_Tuple_of_GenTraversableLike[A, Repr <: GenTraversable[A], B](self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])): Enriched_zipSafe_Tuple_of_GenTraversableLike[A, Repr, B] =
    new Enriched_zipSafe_Tuple_of_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // sliding2: Iterator[(A,A)]
  //   - slide over this collection to produce pairs.
  //   - Functionally equivalent to:
  //         this.sliding(2).map{Seq(a,b) => (a,b)}
  //////////////////////////////////////////////////////

  class Enriched_sliding2_Iterator[A](self: Iterator[A]) {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.sliding(2).map(_.toTuple2)
  }
  implicit def enrich_sliding2_Iterator[A](self: Iterator[A]): Enriched_sliding2_Iterator[A] =
    new Enriched_sliding2_Iterator(self)

  class Enriched_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.toIterator.sliding2()
  }
  implicit def enrich_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_sliding2_GenTraversableLike[A, Repr] =
    new Enriched_sliding2_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // mapValues(f: U => R): Iterator[(A, B)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => k -> f(v)}
  //////////////////////////////////////////////////////

  class Enriched_mapValues_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapValues[R](f: U => R): Iterator[(T, R)] = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }
  }
  implicit def enrich_mapValues_Iterator[T, U](self: Iterator[(T, U)]): Enriched_mapValues_Iterator[T, U] =
    new Enriched_mapValues_Iterator(self)

  //////////////////////////////////////////////////////
  // mapVals(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Functionally equivalent to:
  //         this.map { case (k,v) => k -> f(v) }
  //////////////////////////////////////////////////////

  class Enriched_mapVals_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += k -> f(v)
      b.result
    }
  }
  implicit def enrich_mapVals_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_mapVals_GenTraversableLike[T, U, Repr] =
    new Enriched_mapVals_GenTraversableLike(self)

  class Enriched_mapVals_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R](f: U => R) = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }
  }
  implicit def enrich_mapVals_Iterator[T, U](self: Iterator[(T, U)]): Enriched_mapVals_Iterator[T, U] =
    new Enriched_mapVals_Iterator(self)

  //////////////////////////////////////////////////////
  // mapKeys(f: T => R): Repr[(R,U)]
  //   - In a collection of pairs, map a function over the first item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => f(k) -> v}
  //////////////////////////////////////////////////////

  class Enriched_mapKeys_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += f(k) -> v
      b.result
    }
  }
  implicit def enrich_mapKeys_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_mapKeys_GenTraversableLike[T, U, Repr] =
    new Enriched_mapKeys_GenTraversableLike(self)

  class Enriched_mapKeys_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R](f: T => R) = new Iterator[(R, U)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        f(k) -> v
      }
    }
  }
  implicit def enrich_mapKeys_Iterator[T, U](self: Iterator[(T, U)]): Enriched_mapKeys_Iterator[T, U] =
    new Enriched_mapKeys_Iterator(self)

  //////////////////////////////////////////////////////
  // groupBy(f: A => K): Repr[(R,U)]
  //   - Make Traversable.groupBy functionality available to Iterator
  //////////////////////////////////////////////////////

  class Enriched_groupBy_Iterator[A](self: Iterator[A]) {
    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, Vector[A]] =
      this.groupBy(f, Vector.newBuilder[A])

    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @param builder	a builder to construct collections of items that have been grouped
     * @return Map from new keys to original items
     */
    def groupBy[K, That <: Iterable[A]](f: A => K, builder: => Builder[A, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[A, That]]
      for (elem: A <- self) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, builder)
        bldr += elem
      }
      val b = Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }
  implicit def enrich_groupBy_Iterator[A](self: Iterator[A]): Enriched_groupBy_Iterator[A] =
    new Enriched_groupBy_Iterator(self)

  //////////////////////////////////////////////////////
  // counts(): Map[A, Int]
  //   - Map each distinct item in the collection to the number of times it appears.
  //////////////////////////////////////////////////////

  class Enriched_counts_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] =
      self.toIterator.groupBy(identity).mapVals(_.size)
  }
  implicit def enrich_counts_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enriched_counts_GenTraversableOnce[A] =
    new Enriched_counts_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs, group by the first item in the pair.
  //////////////////////////////////////////////////////

  class Enriched_groupByKey_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That](implicit ev: A <:< (T, U), bf: CanBuildFrom[Repr, U, That]): Map[T, That] =
      self.groupBy(_._1).map { case (k, vs) => k -> (bf(self.asInstanceOf[Repr]) ++= vs.map(_._2)).result }
  }
  implicit def enrich_groupByKey_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]): Enriched_groupByKey_TraversableLike[A, Repr] =
    new Enriched_groupByKey_TraversableLike(self)

  class Enriched_groupByKey_Iterator[A](self: Iterator[A]) {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, Vector[U]] =
      this.groupByKey(Vector.newBuilder[U])

    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @param builder a builder to construct collections of items that have been grouped
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      self.groupBy(_._1).mapVals(v => (builder ++= v.map(_._2)).result)
  }
  implicit def enrich_groupByKey_Iterator[A](self: Iterator[A]): Enriched_groupByKey_Iterator[A] =
    new Enriched_groupByKey_Iterator(self)

  //////////////////////////////////////////////////////
  // +++(other: TraversableOnce[(T,Traversable[S])]): Repr[(T,Traversable[S])]
  //   - Given two collections of pairs (T,Traversable[S]), combine into
  //     a single collection such that all values associated with the same
  //     key are concatenated.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
  //////////////////////////////////////////////////////

  class Enriched_$plus$plus$plus_TraversableLike_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]) {
    /**
     * Given two collections of pairs (T,Traversable[S]), combine into
     * a single collection such that all values associated with the same
     * key are concatenated.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def +++[That1 <: Traversable[(T, TraversableLike[S, Repr2])], That2](other: TraversableOnce[(T, TraversableLike[S, Repr2])])(implicit bf2: CanBuildFrom[Repr2, S, That2], bf1: CanBuildFrom[Repr1, (T, That2), That1]) = {
      val grouped = (self.toIterator ++ other).groupByKey
      val b = bf1(grouped.asInstanceOf[Repr1])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) {
        val b2 = bf2()
        for (v <- vs) b2 ++= v
        b += k -> b2.result
      }
      b.result
    }
  }
  implicit def enrich_$plus$plus$plus_TraversableLike_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]): Enriched_$plus$plus$plus_TraversableLike_Traversable[T, S, Repr2, Repr1] =
    new Enriched_$plus$plus$plus_TraversableLike_Traversable(self)

  //////////////////////////////////////////////////////
  // +++[U:Numeric](other: Traversable[(T,U)]): Repr[(T,U)]
  //   - Given two collections of pairs (T,U:Numeric), combine into
  //     a single collection such that all values associated with the same
  //     key are summed.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).sum)
  //////////////////////////////////////////////////////

  class Enriched_$plus$plus$plus_TraversableLike_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: TraversableLike[(T, U), Repr]) {
    /**
     * Given two collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def +++[That <: Traversable[(T, U)]](other: TraversableOnce[(T, U)])(implicit bf: CanBuildFrom[Repr, (T, U), That]) = {
      val grouped = (self.toIterator ++ other).groupByKey
      val b = bf(grouped.asInstanceOf[Repr])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) b += k -> vs.sum
      b.result
    }
  }
  implicit def enrich_$plus$plus$plus_TraversableLike_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: TraversableLike[(T, U), Repr]) = new Enriched_$plus$plus$plus_TraversableLike_Numeric(self)

  //  class Enriched_$plus$plus$plus_Iterator[T, U](self: Iterator[(T, U)]) {
  //    /**
  //     * In a collection of pairs, map a function over the second item of each
  //     * pair.
  //     *
  //     * @param f	function to map over the second item of each pair
  //     * @return a collection of pairs
  //     */
  //    def +++[R] = self.mapValues(f)
  //  }
  //  implicit def enrich_$plus$plus$plus_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_$plus$plus$plus_Iterator(self)

  //////////////////////////////////////////////////////
  // flattenByKey: Repr[(T,Traversable[S])]
  //   - Reduce a collection of collections of pairs (T,Traversable[S]), into
  //     a single collection such that all values associated with the same
  //     key are concatenated.
  //   - Functionally equivalent to:
  //         this.flatten.groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  class Enriched_flattenByKey_TraversableOnce_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableOnce[TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]]) {
    /**
     * Reduce a collection of collections of pairs (T,Traversable[S]), into
     * a single collection such that all values associated with the same
     * key are concatenated.
     *
     * @return a collection of pairs
     */
    def flattenByKey[That1 <: Traversable[(T, TraversableLike[S, Repr2])], That2](implicit bf2: CanBuildFrom[Repr2, S, That2], bf1: CanBuildFrom[Repr1, (T, That2), That1]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf1(grouped.asInstanceOf[Repr1])
      for ((k, vs) <- grouped) {
        val b2 = bf2()
        for (v <- vs) b2 ++= v
        b += k -> b2.result
      }
      b.result
    }
  }
  implicit def enrich_flattenByKey_TraversableOnce_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](self: TraversableOnce[TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]]) = new Enriched_flattenByKey_TraversableOnce_Traversable(self)

  //////////////////////////////////////////////////////
  // sumByKey[U:Numeric](other: Traversable[(T,U)]): Repr[(T,U)]
  //   - Given a collection of collections of pairs (T,U:Numeric), combine into
  //     a single collection such that all values associated with the same
  //     key are summed.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).sum)
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  class Enriched_sumByKey_GenTraversableOnce_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: GenTraversableOnce[TraversableLike[(T, U), Repr]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey[That <: Traversable[(T, U)]](implicit bf: CanBuildFrom[Repr, (T, U), That]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf(grouped.asInstanceOf[Repr])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) b += k -> vs.sum
      b.result
    }
  }
  implicit def enrich_sumByKey_GenTraversableOnce_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](self: GenTraversableOnce[TraversableLike[(T, U), Repr]]) = new Enriched_sumByKey_GenTraversableOnce_Numeric(self)

  class Enriched_sumByKey_GenTraversableOnce_Iterator[T, U: Numeric](self: GenTraversableOnce[Iterator[(T, U)]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey = self.toIterator.flatten.groupByKey.mapVals(_.sum)
  }
  implicit def enrich_sumByKey_GenTraversableOnce_Iterator[T, U: Numeric](self: GenTraversableOnce[Iterator[(T, U)]]) = new Enriched_sumByKey_GenTraversableOnce_Iterator(self)

  //////////////////////////////////////////////////////
  // mapt[A,B,R](f: (A,B) => R): Repr[R]
  //   - map over a Tuple2
  //   - same as `xs.map { case (x,y) => f(x,y) } `
  //////////////////////////////////////////////////////

  class Enriched_mapt_2_GenTraversableLike[A, B, Repr <: GenTraversable[(A, B)]](self: GenTraversableLike[(A, B), Repr]) {
    def mapt[R, That](f: (A, B) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      val fTupled = f.tupled
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (t <- self) b += fTupled(t)
      b.result
    }
  }
  implicit def enrich_mapt_2_GenTraversableLike[A, B, Repr <: GenTraversable[(A, B)]](self: GenTraversableLike[(A, B), Repr]) = new Enriched_mapt_2_GenTraversableLike(self)

  class Enriched_mapt_3_GenTraversableLike[A, B, C, Repr <: GenTraversable[(A, B, C)]](self: GenTraversableLike[(A, B, C), Repr]) {
    def mapt[R, That](f: (A, B, C) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      val fTupled = f.tupled
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for (t <- self) b += fTupled(t)
      b.result
    }
  }
  implicit def enrich_mapt_3_GenTraversableLike[A, B, C, Repr <: GenTraversable[(A, B, C)]](self: GenTraversableLike[(A, B, C), Repr]) = new Enriched_mapt_3_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // unzip(): (Iterator[A], Iterator[B])
  //   - Extend unzip functionality to Iterator
  //////////////////////////////////////////////////////

  class Enriched_unzip_Iterator[T, U](self: Iterator[(T, U)]) {
    def unzip(): (Vector[T], Vector[U]) =
      this.unzip(Vector.newBuilder[T], Vector.newBuilder[U])

    def unzip[ThatT <: Iterable[T], ThatU <: Iterable[U]](tBuilder: => Builder[T, ThatT], uBuilder: => Builder[U, ThatU]): (ThatT, ThatU) = {
      val tBldr = tBuilder
      val uBldr = uBuilder
      for ((t, u) <- self) {
        tBldr += t
        uBldr += u
      }
      (tBldr.result, uBldr.result)
    }
  }
  implicit def enrich_unzip_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_unzip_Iterator(self)

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //////////////////////////////////////////////////////

  class Enriched_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]) {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * @return an iterator of pairs
     */
    def ungroup() = self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  }
  implicit def enrich_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]) = new Enriched_ungroup_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

  class Enriched_takeSub_Iterator[A, R <: GenIterable[A]](self: Iterator[GenIterableLike[A, R]]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub(n: Int): Iterator[R] = {
      if (self.isEmpty) {
        self.asInstanceOf[Iterator[R]]
      }
      else {
        new Iterator[R] {
          private var nextElement: R = self.next.asInstanceOf[R]
          private var total: Int = nextElement.size

          override def hasNext = total <= n

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next.asInstanceOf[R]
                total += nextElement.size
              }
              else
                total = n + 1
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }
  implicit def enrich_takeSub_Iterator[A, R <: GenIterable[A]](self: Iterator[GenIterableLike[A, R]]) = new Enriched_takeSub_Iterator(self)

  class Enriched_takeSub_GenTraversableLike[A, R <: GenIterable[A], Repr <: GenTraversable[GenIterable[A]]](self: GenTraversableLike[GenIterableLike[A, R], Repr]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub[That](n: Int)(implicit bf: CanBuildFrom[Repr, R, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      for (x <- self.toIterator.takeSub(n)) b += x
      b.result
    }
  }
  implicit def enrich_takeSub_GenTraversableLike[A, R <: GenIterable[A], Repr <: GenTraversable[GenIterable[A]]](self: GenTraversableLike[GenIterableLike[A, R], Repr]) = new Enriched_takeSub_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // dropRightWhile(p: A => Boolean): Repr
  //////////////////////////////////////////////////////

  class Enriched_dropRightWhile_IterableLike[A, Repr <: Iterable[A]](self: IterableLike[A, Repr]) {
    def dropRightWhile[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      val buffer = collection.mutable.Buffer[A]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }
  implicit def enrich_dropRightWhile_IterableLike[A, Repr <: Iterable[A]](self: IterableLike[A, Repr]): Enriched_dropRightWhile_IterableLike[A, Repr] =
    new Enriched_dropRightWhile_IterableLike(self)

  class Enriched_dropRightWhile_String(self: String) {
    def dropRightWhile(p: Char => Boolean): String = {
      val b = stringCanBuildFrom()
      val buffer = collection.mutable.Buffer[Char]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }
  implicit def enrich_dropRightWhile_String(self: String): Enriched_dropRightWhile_String =
    new Enriched_dropRightWhile_String(self)

  //////////////////////////////////////////////////////
  // +:(elem: B): Iterator[B]
  //   - Prepend an element to the iterator
  // :+(elem: B): Iterator[B]
  //   - Append an element to the end of the iterator
  //////////////////////////////////////////////////////

  class Enriched_prependAppend_Iterator[A](self: Iterator[A]) {
    /**
     * Prepend an item to the front of the iterator
     *
     * @param elem	the item to be prepended
     * @return a new iterator
     */
    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    /**
     * Append an item to the end of the iterator
     *
     * @param elem	the item to be appended
     * @return a new iterator
     */
    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)
  }
  implicit def enrich_prependAppend_Iterator[A](self: Iterator[A]): Enriched_prependAppend_Iterator[A] =
    new Enriched_prependAppend_Iterator(self)

  //////////////////////////////////////////////////////
  // avg(): A
  //   - Find the average (mean) of this collection of numbers
  //////////////////////////////////////////////////////

  class Enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg(implicit num: Fractional[A]) = {
      val (total, count) = self.toIterator.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
  }
  implicit def enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enrich_avg_GenTraversableOnce[A] =
    new Enrich_avg_GenTraversableOnce(self)

  class Enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      val (total, count) = self.toIterator.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
  }
  implicit def enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]): Enrich_avg_Int_GenTraversableOnce =
    new Enrich_avg_Int_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  class Enriched_normalize_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit num: Fractional[A], bf: CanBuildFrom[Repr, A, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum
      for (x <- self) b += num.div(x, total)
      b.result
    }
  }
  implicit def enrich_normalize_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]): Enriched_normalize_GenTraversableLike[A, Repr] =
    new Enriched_normalize_GenTraversableLike(self)

  class Enriched_normalize_Int_GenTraversableLike[Repr <: GenTraversable[Int]](self: GenTraversableLike[Int, Repr]) {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit bf: CanBuildFrom[Repr, Double, That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sum.toDouble
      for (x <- self) b += x / total
      b.result
    }
  }
  implicit def enrich_normalize_Int_GenTraversableLike[Repr <: GenTraversable[Int]](self: GenTraversableLike[Int, Repr]): Enriched_normalize_Int_GenTraversableLike[Repr] =
    new Enriched_normalize_Int_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // normalizeValues(): Repr[(T,U)]
  //   - Normalize this values in this collection of pairs
  //////////////////////////////////////////////////////

  class Enriched_normalizeValues_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit num: Fractional[U], bf: CanBuildFrom[Repr, (T, U), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sumBy(_._2)
      for ((k, v) <- self) b += k -> num.div(v, total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]): Enriched_normalizeValues_GenTraversableLike[T, U, Repr] =
    new Enriched_normalizeValues_GenTraversableLike(self)

  class Enriched_normalizeValues_Int_GenTraversableLike[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.sumBy(_._2).toDouble
      for ((k, v) <- self) b += k -> (v / total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_Int_GenTraversableLike[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]): Enriched_normalizeValues_Int_GenTraversableLike[T, Repr] =
    new Enriched_normalizeValues_Int_GenTraversableLike(self)

  //////////////////////////////////////////////////////
  // shuffle
  //////////////////////////////////////////////////////

  class Enriched_shuffle_GenTraversableOnce[T, CC[X] <: TraversableOnce[X]](xs: CC[T]) {
    def shuffle(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
      bf(xs) ++= Random.shuffle(xs) result
    }
  }
  implicit def enrich_shuffle_GenTraversableOnce[T, CC[X] <: TraversableOnce[X]](xs: CC[T]) = new Enriched_shuffle_GenTraversableOnce(xs)

  //  class ReversableIterableMap[A, B](map: Map[A, GenTraversableOnce[B]]) {
  //    def reverse(): Map[B, GenTraversableOnce[A]] =
  //      map.ungroup.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //  implicit def map2reversableIterableMap[A, B](map: Map[A, GenTraversableOnce[B]]) = new ReversableIterableMap(map)
  //
  //  class ReversableMap[A, B](map: Map[A, B]) {
  //    def reverse(): Map[B, Iterable[A]] =
  //      map.toIndexedSeq.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //  implicit def map2reversableMap[A, B](map: Map[A, B]) = new ReversableMap(map)

  /**
   * This Set implementation always returns 'true' from its 'contains' method.
   */
  class UniversalSet[A] extends Set[A] {
    override def contains(key: A): Boolean = true
    override def iterator: Iterator[A] = sys.error("not implemented")
    override def +(elem: A): UniversalSet[A] = sys.error("not implemented")
    override def -(elem: A): UniversalSet[A] = sys.error("not implemented")
    override def toString() = "UniversalSet()"
  }
  object UniversalSet {
    def apply[A]() = new UniversalSet[A]
  }

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  class EnrichedWithToMMap[K, V](self: TraversableOnce[(K, V)]) {
    def toMMap =
      if (self.isEmpty) mutable.Map.empty[K, V]
      else mutable.Map.newBuilder[K, V] ++= self result
  }
  implicit def addToMMap[K, V](self: TraversableOnce[(K, V)]) = new EnrichedWithToMMap(self)

  class EnrichedWithToVector[A](self: TraversableOnce[A]) {
    def toVector =
      if (self.isEmpty) Vector.empty[A]
      else Vector.newBuilder[A] ++= self result
  }
  implicit def addToVector[A](self: TraversableOnce[A]) = new EnrichedWithToVector(self)

  class EnrichedArrayWithToVector[A](self: Array[A]) {
    def toVector: Vector[A] =
      if (self.isEmpty) Vector.empty[A]
      else Vector.newBuilder[A] ++= self result
  }
  implicit def addToVectorToArray[A](self: Array[A]): EnrichedWithToVector[A] = new EnrichedWithToVector(self)
}
