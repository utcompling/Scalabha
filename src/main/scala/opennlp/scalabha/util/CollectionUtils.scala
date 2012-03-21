package opennlp.scalabha.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.TraversableLike
import scala.collection.GenMap
import scala.collection.mutable
import scala.collection.GenIterable
import scala.collection.GenIterableLike

object CollectionUtils {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  class Enrich_toTuple_Seq[A](elements: Seq[A]) {
    def toTuple2 = elements match { case Seq(a, b) => (a, b) }
    def toTuple3 = elements match { case Seq(a, b, c) => (a, b, c) }
    def toTuple4 = elements match { case Seq(a, b, c, d) => (a, b, c, d) }
    def toTuple5 = elements match { case Seq(a, b, c, d, e) => (a, b, c, d, e) }
  }
  implicit def enriched_toTuple_Seq[A](elements: Seq[A]) = new Enrich_toTuple_Seq(elements)

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
  implicit def enriched_countCompare_GenTraversableOnce[A](self: GenTraversableOnce[A]) = new Enriched_countCompare_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // split(delim: A): List[List[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  class Enriched_split_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Split this collection on each occurrence of the delimiter.  Delimiters
     * do not appear in the output.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A) = {
      (self.foldLeft(List[List[A]]()) {
        case (Nil, `delim`) => Nil
        case (Nil, item) => (item :: Nil) :: Nil
        case (cur :: accum, `delim`) => List[A]() :: cur :: accum
        case (cur :: accum, item) => (item :: cur) :: accum
      }).reverse.map(_.reverse)
    }
  }
  implicit def enriched_split_GenTraversableOnce[A](self: GenTraversableOnce[A]) = new Enriched_split_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // splitAt(n: Int) 
  //   - Split this collection at the specified index 
  //   - Inspired by String.splitAt
  //////////////////////////////////////////////////////

  class Enriched_splitAt_Iterator[A](self: Iterator[A]) {
    /**
     * Split this collection at the specified index.
     *
     * Inspired by String.splitAt
     *
     * @param n	The index at which to split the collection
     * @return	a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      var accum: List[A] = Nil
      var i = 0
      while (i < n && self.hasNext) {
        accum ::= self.next
        i += 1
      }
      (accum.reverse.iterator, self)
    }
  }
  implicit def enriched_splitAt_Iterator[A](self: Iterator[A]) = new Enriched_splitAt_Iterator(self)

  //////////////////////////////////////////////////////
  // sumMap[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  class Enriched_sumMap_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f	A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumMap[B](f: A => B)(implicit num: Numeric[B]): B = {
      val itr = self.toIterator
      var accum = num.zero
      while (itr.hasNext)
        accum = num.plus(accum, f(itr.next))
      return accum
    }
  }
  implicit def enriched_sumMap_GenTraversableOnce[A](self: GenTraversableOnce[A]) = new Enriched_sumMap_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[B]
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
  implicit def enriched_mapTo_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) = new Enriched_mapTo_GenTraversableLike(self)

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
  implicit def enriched_mapTo_Iterator[A](self: Iterator[A]) = new Enriched_mapTo_Iterator(self)

  //////////////////////////////////////////////////////
  // zipEqual(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  class Enriched_zipEqual_Iterator[A](self: Iterator[A]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipEqual[B](that: TraversableOnce[B]) = {
      val thatItr = that.toIterator
      new Iterator[(A, B)] {
        def hasNext = {
          if (self.hasNext) {
            if (thatItr.hasNext) true
            else throw new RuntimeException("Attempting to zipEqual collections of different lengths.")
          }
          else {
            if (thatItr.hasNext) throw new RuntimeException("Attempting to zipEqual collections of different lengths.")
            else false
          }
        }
        def next() = {
          this.hasNext
          (self.next, thatItr.next)
        }
      }
    }
  }
  implicit def enriched_zipEqual_Iterator[A](self: Iterator[A]) = new Enriched_zipEqual_Iterator(self)

  class Enriched_zipEqual_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipEqual[A1 >: A, B, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipEqual that.toIterator)
      b.result
    }
  }
  implicit def enriched_zipEqual_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) = new Enriched_zipEqual_GenTraversableLike(self)

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
  implicit def enriched_sliding2_Iterator[A](self: Iterator[A]) = new Enriched_sliding2_Iterator(self)

  class Enriched_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.toIterator.sliding2()
  }
  implicit def enriched_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) = new Enriched_sliding2_GenTraversableLike(self)

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
    def mapValues[R](f: U => R) = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }
  }
  implicit def enrich_mapValues_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_mapValues_Iterator(self)

  //////////////////////////////////////////////////////
  // mapValuesStrict(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => k -> f(v)}
  //////////////////////////////////////////////////////

  class Enriched_mapValuesStrict_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapValuesStrict[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      for ((k, v) <- self) b += k -> f(v)
      b.result
    }
  }
  implicit def enriched_mapValuesStrict_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) = new Enriched_mapValuesStrict_GenTraversableLike(self)

  class Enriched_mapValuesStrict_Iterator[T, U](self: Iterator[(T, U)]) {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapValuesStrict[R](f: U => R) = self.mapValues(f)
  }
  implicit def enrich_mapValuesStrict_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_mapValuesStrict_Iterator(self)

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
  implicit def enrich_mapKeys_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) = new Enriched_mapKeys_GenTraversableLike(self)

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
  implicit def enrich_mapKeys_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_mapKeys_Iterator(self)

  //////////////////////////////////////////////////////
  // groupBy(f: A => K): Repr[(R,U)]
  //   - Make List.groupBy functionality available to Iterator
  //////////////////////////////////////////////////////

  class Enriched_groupBy_Iterator[A](self: Iterator[A]) {
    /**
     * Same functionality as List.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, List[A]] =
      this.groupBy(f, ListBuffer[A]())

    /**
     * Same functionality as List.groupBy(f)
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
  implicit def enriched_groupBy_Iterator[A](self: Iterator[A]) = new Enriched_groupBy_Iterator(self)

  //////////////////////////////////////////////////////
  // counts(): Map[A, Int]
  //   - Map each distinct item in the collection to the number of times it appears.
  //////////////////////////////////////////////////////

  class Enriched_counts_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] =
      self.groupBy(identity).mapValuesStrict(_.size)
  }
  implicit def enrich_counts_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) = new Enriched_counts_TraversableLike[A, Repr](self)

  class Enriched_counts_Iterator[A](self: Iterator[A]) {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] =
      self.groupBy(identity).mapValuesStrict(_.size)
  }
  implicit def enrich_counts_Iterator[A](self: Iterator[A]) = new Enriched_counts_Iterator(self)

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
      self.groupBy(_._1).mapValuesStrict(_.map(_._2).asInstanceOf[That])
  }
  implicit def enriched_groupByKey_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) = new Enriched_groupByKey_TraversableLike[A, Repr](self)

  class Enriched_groupByKey_Iterator[A](self: Iterator[A]) {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, List[U]] =
      this.groupByKey(ListBuffer[U]())

    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @param builder a builder to construct collections of items that have been grouped
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      self.groupBy(_._1).mapValuesStrict(v => (builder ++= v.map(_._2)).result)
  }
  implicit def enriched_groupByKey_Iterator[A](self: Iterator[A]) = new Enriched_groupByKey_Iterator(self)

  //////////////////////////////////////////////////////
  // zip(that: TraversableOnce[B]): Iterator[(A, B)]
  //   - Extend zip functionality to Iterator
  //////////////////////////////////////////////////////

  class Enriched_zip_Iterator[A](self: Iterator[A]) {
    /**
     * Same functionality as List.zip(that)
     *
     * @param that	collection to zip with
     * @return Iterator of zipped pairs
     */
    def zip[B](that: TraversableOnce[B]): Iterator[(A, B)] =
      this.zip(that.toIterator)
  }
  implicit def enriched_zip_Iterator[A](self: Iterator[A]) = new Enriched_zip_Iterator(self)

  //////////////////////////////////////////////////////
  // zip(that: TraversableOnce[B]): Iterator[(A, B)]
  //   - Extend zip functionality to Iterator
  //////////////////////////////////////////////////////

  class Enriched_unzip_Iterator[T, U](self: Iterator[(T, U)]) {
    def unzip(): (List[T], List[U]) =
      this.unzip(ListBuffer[T](), ListBuffer[U]())

    def unzip[ThatT <: Iterable[T], ThatU <: Iterable[U]](tBuilder: => Builder[T, ThatT], uBuilder: => Builder[U, ThatU]): (ThatT, ThatU) = {
      for ((t, u) <- self) {
        tBuilder += t
        uBuilder += u
      }
      (tBuilder.result, uBuilder.result)
    }
  }
  implicit def enrich_unzip_Iterator[T, U](self: Iterator[(T, U)]) = new Enriched_unzip_Iterator(self)

  //////////////////////////////////////////////////////
  // flattenOver(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - TODO: This might be better called "ungroup"
  //////////////////////////////////////////////////////

  class Enrich_flattenOver_GenTraversableOnce[A, B](self: GenMap[A, GenTraversableOnce[B]]) {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * @return an iterator of pairs
     */
    def flattenOver() = self.iterator.flatMap { case (a, bs) => bs.toIterator.map((a, _)) }
  }
  implicit def enrich_flattenOver_GenTraversableOnce[A, B](self: GenMap[A, GenTraversableOnce[B]]) = new Enrich_flattenOver_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

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
      for (x <- enriched_takeSub_Iterator(self.toIterator).takeSub(n)) b += x
      b.result
    }
  }
  implicit def enrich_takeSub_GenTraversableLike[A, R <: GenIterable[A], Repr <: GenTraversable[GenIterable[A]]](self: GenTraversableLike[GenIterableLike[A, R], Repr]) = new Enriched_takeSub_GenTraversableLike(self)

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
  implicit def enriched_takeSub_Iterator[A, R <: GenIterable[A]](self: Iterator[GenIterableLike[A, R]]) = new Enriched_takeSub_Iterator(self)

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
  implicit def enrich_prependAppend_Iterator[A](self: Iterator[A]) = new Enriched_prependAppend_Iterator(self)

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
  implicit def enrich_avg_GenTraversableOnce[A](self: GenTraversableOnce[A]) = new Enrich_avg_GenTraversableOnce(self)

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
  implicit def enrich_avg_Int_GenTraversableOnce(self: GenTraversableOnce[Int]) = new Enrich_avg_Int_GenTraversableOnce(self)

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
  implicit def enrich_normalize_GenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) = new Enriched_normalize_GenTraversableLike(self)

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
  implicit def enrich_normalize_Int_GenTraversableLike[Repr <: GenTraversable[Int]](self: GenTraversableLike[Int, Repr]) = new Enriched_normalize_Int_GenTraversableLike(self)

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
      val total = self.map(_._2).sum
      for ((k, v) <- self) b += k -> num.div(v, total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_GenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) = new Enriched_normalizeValues_GenTraversableLike(self)

  class Enriched_normalizeValues_Int_GenTraversableLike[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]) {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      val total = self.map(_._2).sum.toDouble
      for ((k, v) <- self) b += k -> (v / total)
      b.result
    }
  }
  implicit def enrich_normalizeValues_Int_GenTraversableLike[T, Repr <: GenTraversable[(T, Int)]](self: GenTraversableLike[(T, Int), Repr]) = new Enriched_normalizeValues_Int_GenTraversableLike(self)

  //  class ReversableIterableMap[A, B](map: Map[A, GenTraversableOnce[B]]) {
  //    def reverse(): Map[B, GenTraversableOnce[A]] =
  //      map.flattenOver.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //  implicit def map2reversableIterableMap[A, B](map: Map[A, GenTraversableOnce[B]]) = new ReversableIterableMap(map)
  //
  //  class ReversableMap[A, B](map: Map[A, B]) {
  //    def reverse(): Map[B, Iterable[A]] =
  //      map.toList.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
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
}
