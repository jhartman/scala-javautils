/**
 * @Author Joshua Hartman
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **/
package org.scala_tools.javautils.j2s

import scala.{Iterator => SIterator}
import java.lang.Iterable
import java.util.{Iterator, Collection}
import org.scala_tools.javautils.Implicits._

//Still looking for a way to unify with the map higher order functions
//object GenericTypes {
//  type GenericCollection = { def add[T](value: T): java.lang.Boolean}
//}
//import GenericTypes._
trait HigherOrderCollectionFunctions[T, CollectionType[X] <: Collection[X]] {
  //trait HigherOrderCollectionFunctions[T, CollectionType <: GenericCollection] {
  def getNewCollection[V]: CollectionType[V]

  def getIterator: Iterator[T]

  def toSeq(): Seq[T] = {
    getIterator.asScala.collect
  }

  def toArray(): Array[T] = {
    toSeq.toArray
  }

  def elements(): Iterator[T] = getIterator

  def foreach(fn: (T) => Unit): Unit = {
    val iterator = getIterator
    while (iterator.hasNext) fn(iterator.next)
  }

  def ++[V >: T](that: Iterable[V]): Iterable[V] = {
    // Copy everything over into a new collection
    val collection = getNewCollection[V]
    val iterator = getIterator

    iterator.foreach(collection add _)
    that.foreach(collection add _)
    collection
  }

  def ++[V >: T](that: scala.Iterable[V]): Iterable[V] = {
    ++(that.asJava)
  }


  def map[V](fn: (T) => V): CollectionType[V] = {
    val iterator = getIterator
    val collection: CollectionType[V] = getNewCollection[V]
    while (iterator.hasNext) {
      val result: V = fn(iterator.next)
      collection.add(result)
    }
    collection
  }

  def flatMap[V](f: (T) => Iterator[V]): CollectionType[V] = {
    val iterator = getIterator
    val collection: CollectionType[V] = getNewCollection[V]
    while (iterator.hasNext) {
      val innerIterator = f(iterator.next)
      while (innerIterator.hasNext) {
        collection.add(innerIterator.next)
      }
    }
    collection
  }

  def foldLeft[V](initial: V)(fn: (V, T) => V): V = {
    foldLeft0(getIterator, fn, initial)
  }

  def filter(pred: (T) => Boolean): CollectionType[T] = {
    val iterator = getIterator
    val collection = getNewCollection[T]
    while (iterator.hasNext) {
      val elem = iterator.next
      if (pred(elem)) collection.add(elem)
    }
    collection
  }

  def partition(pred: (T) => Boolean): (CollectionType[T], CollectionType[T]) = {
    val iterator = getIterator
    val satisfiesPredicate = getNewCollection[T]
    val doesNotSatisfyPredicate = getNewCollection[T]

    while (iterator.hasNext) {
      val elem = iterator.next
      if (pred(elem)) satisfiesPredicate.add(elem)
      else doesNotSatisfyPredicate.add(elem)
    }

    (satisfiesPredicate, doesNotSatisfyPredicate)
  }

  private def foldLeft0[T, V](iterator: Iterator[T], fn: (V, T) => V, accumulator: V): V = {
    if (iterator.hasNext)
      foldLeft0(iterator, fn, fn(accumulator, iterator.next))
    else
      accumulator
  }
}