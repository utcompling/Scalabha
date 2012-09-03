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
import scala.collection.immutable
import scala.collection.mutable.Builder
import scala.util.Random

object CollectionUtil {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  class Enriched_toTuple_Seq[A](seq: Seq[A]) {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case _ => throw new AssertionError("Cannot convert sequence of length %s into Tuple2".format(seq.size)) }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case _ => throw new AssertionError("Cannot convert sequence of length %s into Tuple3".format(seq.size)) }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case _ => throw new AssertionError("Cannot convert sequence of length %s into Tuple4".format(seq.size)) }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case _ => throw new AssertionError("Cannot convert sequence of length %s into Tuple5".format(seq.size)) }
  }
  implicit def enrich_toTuple_Seq[A](seq: Seq[A]): Enriched_toTuple_Seq[A] =
    new Enriched_toTuple_Seq(seq)

  class Enriched_toTuple_Array[A](seq: Array[A]) {
    def toTuple2 = seq match { case Array(a, b) => (a, b); case _ => throw new AssertionError("Cannot convert array of length %s into Tuple2".format(seq.size)) }
    def toTuple3 = seq match { case Array(a, b, c) => (a, b, c); case _ => throw new AssertionError("Cannot convert array of length %s into Tuple3".format(seq.size)) }
    def toTuple4 = seq match { case Array(a, b, c, d) => (a, b, c, d); case _ => throw new AssertionError("Cannot convert array of length %s into Tuple4".format(seq.size)) }
    def toTuple5 = seq match { case Array(a, b, c, d, e) => (a, b, c, d, e); case _ => throw new AssertionError("Cannot convert array of length %s into Tuple5".format(seq.size)) }
  }
  implicit def enrich_toTuple_Array[A](seq: Array[A]): Enriched_toTuple_Array[A] =
    new Enriched_toTuple_Array(seq)

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
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs (k,v), create a map from each `k` to the  
  //     collection of `v`s with which it is associated.
  //   - Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
  //////////////////////////////////////////////////////

  class Enriched_groupByKey_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]) {
    /**
     * For a collection of pairs (k,v), create a map from each `k` to the
     * collection of `v`s with which it is associated.
     *
     * Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
     *
     * @return Map from `k`s to collections of `v`s
     */
    def groupByKey[K, V, That](implicit ev: A <:< (K, V), bf: CanBuildFrom[Repr, V, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[V, That]]
      for ((key, value) <- self.map(ev)) {
        val bldr = m.getOrElseUpdate(key, bf(self.asInstanceOf[Repr]))
        bldr += value
      }
      val b = immutable.Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }
  implicit def enrich_groupByKey_TraversableLike[A, Repr <: Traversable[A]](self: TraversableLike[A, Repr]): Enriched_groupByKey_TraversableLike[A, Repr] =
    new Enriched_groupByKey_TraversableLike(self)

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  //////////////////////////////////////////////////////

  class Enriched_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]) {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
     *
     * @return an iterator of pairs
     */
    def ungroup() = self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  }
  implicit def enrich_ungroup_GenTraversableOnce[A, B](self: GenTraversableOnce[(A, GenTraversableOnce[B])]): Enriched_ungroup_GenTraversableOnce[A, B] =
    new Enriched_ungroup_GenTraversableOnce(self)

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  class Enriched_toVector_GenTraversableOnce[A](self: GenTraversableOnce[A]) {
    def toVector =
      if (self.isEmpty) Vector.empty[A]
      else Vector.newBuilder[A] ++= self.toIterator result
  }
  implicit def enrich_toVector_GenTraversableOnce[A](self: GenTraversableOnce[A]): Enriched_toVector_GenTraversableOnce[A] =
    new Enriched_toVector_GenTraversableOnce(self)

  class Enriched_toVector_Array[A](self: Array[A]) {
    def toVector: Vector[A] =
      if (self.isEmpty) Vector.empty[A]
      else Vector.newBuilder[A] ++= self result
  }
  implicit def addToVectorToArray[A](self: Array[A]): Enriched_toVector_Array[A] = new Enriched_toVector_Array(self)

}
