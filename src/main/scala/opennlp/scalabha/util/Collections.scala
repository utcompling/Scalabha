package opennlp.scalabha.util

import scala.collection.GenTraversableOnce
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.MutableMapFactory
import scala.collection.mutable.HashMap

object Collections {

  class MemoMap[A, B](startEntries: Map[A, B], default: A => B) extends (A => B) with Iterable[(A, B)] { //mutable.Map[A, B] {
    private[this] val cache = mutable.Map[A, B]() ++ startEntries
    override def apply(key: A): B = cache.getOrElseUpdate(key, default(key))
    override def iterator: Iterator[(A, B)] = cache.iterator
  }

  //
  //
  //

  class History[T] private (length: Int, lag: Int) {
    private[this] val q = scala.collection.mutable.Queue[T]()
    def ::=(t: T) = { q.enqueue(t); if (q.length > length) q.dequeue(); this }
    def :::=(ts: GenTraversableOnce[T]) = { for (t <- ts) this ::= t; this }
    def head = q.last
    def iterator = q.reverseIterator.grouped(lag + 1).map(_(0))
  }
  object History {
    def apply[T](length: Int, lag: Int): History[T] = new History[T](if (length > 0) length else 1, lag)
    def apply[T](length: Int, e: T, lag: Int): History[T] = History(length, lag) ::= e
  }

}
