package opennlp.scalabha.util

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.TraversableLike
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.GenMap

object CollectionUtils {

  class EnrichedGenTraversableOnce[A](self: GenTraversableOnce[A]) {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param   p       the predicate used to test elements.
     *   @param   count   the test value that gets compared with the count.
     *   @return  A value `x` where
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

    def split(delim: A) = {
      (self.foldLeft(List[List[A]]()) {
        case (Nil, `delim`) => Nil
        case (Nil, item) => (item :: Nil) :: Nil
        case (cur :: accum, `delim`) => List[A]() :: cur :: accum
        case (cur :: accum, item) => (item :: cur) :: accum
      }).reverse.map(_.reverse)
    }

    def mapSum[B](f: A => B)(implicit num: Numeric[B]): B =
      self.toIterator.map(f).sum
  }
  implicit def enrichGenTraversableOnce[A](self: GenTraversableOnce[A]) = new EnrichedGenTraversableOnce(self)

  class EnrichedGenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) {
    val repr = self.asInstanceOf[Repr]

    /**
     * map(x => x -> f(x))
     */
    def mapTo[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      val b = bf(repr)
      b.sizeHint(self.size)
      for (x <- self) b += x -> f(x)
      b.result
    }

    def zipEqual[A1 >: A, B, That](that: GenTraversable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(repr)
      b ++= (enrichIterator(self.toIterator) zipEqual that.toIterator)
      b.result
    }

    def sliding2[B >: A](): Iterator[(B, B)] =
      self.toIterator.sliding2()
  }
  implicit def enrichGenTraversableLike[A, Repr <: GenTraversable[A]](self: GenTraversableLike[A, Repr]) = new EnrichedGenTraversableLike(self)

  class EnrichedPairGenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) {
    val repr = self.asInstanceOf[Repr]

    /**
     * map{case (k,v) => k -> f(v)}
     */
    def mapValuesStrict[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      val b = bf(repr)
      b.sizeHint(self.size)
      for ((k, v) <- self) b += k -> f(v)
      b.result
    }

    /**
     * map{case (k,v) => f(k) -> v}
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      val b = bf(repr)
      b.sizeHint(self.size)
      for ((k, v) <- self) b += f(k) -> v
      b.result
    }
  }
  implicit def enrichPairGenTraversableLike[T, U, Repr <: GenTraversable[(T, U)]](self: GenTraversableLike[(T, U), Repr]) = new EnrichedPairGenTraversableLike(self)

  class EnrichedTraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    val repr = self.asInstanceOf[Repr]

    def counts(): Map[A, Int] =
      self.groupBy(identity).mapValuesStrict(_.size)

    def groupByKey[T, U, That](implicit ev: A <:< (T, U), bf: CanBuildFrom[Repr, U, That]): Map[T, That] =
      self.groupBy(_._1).mapValuesStrict(_.map(_._2).asInstanceOf[That])
  }
  implicit def enrichTraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) = new EnrichedTraversableLike[A, Repr](self)

  class EnrichedIterator[A](self: Iterator[A]) {
    def groupBy[K](f: A => K): Map[K, List[A]] =
      this.groupBy(f, ListBuffer[A]())

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

    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, List[U]] =
      this.groupByKey(ListBuffer[U]())

    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      this.groupBy(_._1).mapValuesStrict(v => (builder ++= v.map(_._2)).result)

    /**
     * map(x => x -> f(x))
     */
    def mapTo[B](f: A => B): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = self.hasNext
      def next() = {
        val x = self.next()
        x -> f(x)
      }
    }

    def counts(): Map[A, Int] =
      this.groupBy(identity).mapValuesStrict(_.size)

    def zipEqual[B](that: TraversableOnce[B]) = {
      val thatItr = that.toIterator
      new Iterator[(A, B)] {
        def hasNext = {
          if (self.hasNext) {
            if (thatItr.hasNext) true
            else throw new RuntimeException("Attempting to zipEqual collections of different lengths.")
          } else {
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

    def zip[B](that: TraversableOnce[B]): Iterator[(A, B)] =
      this.zip(that.toIterator)

    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)

    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      var accum: List[A] = Nil
      var i = 0
      while (i < n && self.hasNext) {
        accum ::= self.next
        i += 1
      }
      (accum.reverse.iterator, self)
    }

    def sliding2[B >: A](): Iterator[(B, B)] =
      self.sliding(2).map { case Seq(a, b) => (a, b) }
  }
  implicit def enrichIterator[A](self: Iterator[A]) = new EnrichedIterator(self)

  class EnrichedPairIterator[T, U](self: Iterator[(T, U)]) {
    /**
     * map{case (k,v) => k -> f(v)}
     */
    def mapValues[R](f: U => R) = new Iterator[(T, R)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        k -> f(v)
      }
    }

    def mapValuesStrict[R](f: U => R) = mapValues(f)

    /**
     * map{case (k,v) => f(k) -> v}
     */
    def mapKeys[R](f: T => R) = new Iterator[(R, U)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, v) = self.next()
        f(k) -> v
      }
    }

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
  implicit def enrichPairIterator[T, U](self: Iterator[(T, U)]) = new EnrichedPairIterator(self)

  class EnrichFlattenOverable[A, B](self: GenMap[A, GenTraversableOnce[B]]) {
    def flattenOver() = self.iterator.flatMap { case (a, bs) => bs.toIterator.map((a, _)) }
  }
  implicit def enrichFlattenOverable[A, B](self: GenMap[A, GenTraversableOnce[B]]) = new EnrichFlattenOverable(self)

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

  /**
   * Return 'tokens', unchanged, if it is longer than 'minLength'.
   * Otherwise, return an empty iterator.
   */
  def getIfValidLength(tokens: Iterator[String], minLength: Int): Iterator[String] = {
    val (pre, suf) = tokens.splitAt(minLength)
    val buf = pre.toList
    if (buf.size == minLength)
      buf.iterator ++ suf
    else
      Iterator()
  }

}
