package org.scala_tools.javautils.j2s

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

import java.util.{Collection, HashMap, Map}
import JImplicits._


//Still looking for a way to unify with the collection higher order functions
//trait MapHigherOrderFunctions[T, V, CollectionType[X, Y] <: java.util.Map[X, Y]] {
trait MapHigherOrderFunctions[T, V, CollectionType[X, Y] <: java.util.Map[X, Y]] {
  def getNewCollection[U, W]: CollectionType[U, W]

  def getIterator: java.util.Iterator[(T, V)]

  def foreach(fn: (T, V) => Unit): Unit = {
    val iterator = getIterator
    while (iterator.hasNext) {
      val elem = iterator.next
      fn(elem._1, elem._2)
    }
  }

  def map[U, W](fn: (T, V) => (U, W)): CollectionType[U, W] = {
    val iterator = getIterator
    val collection = getNewCollection[U, W]
    while (iterator.hasNext) {
      val elem = iterator.next
      val result = fn(elem._1, elem._2)
      collection.put(result._1, result._2)
    }
    collection
  }

  def flatMap[U, W](fn: (T, V) => Iterator[(U, W)]): CollectionType[U, W] = {
    val iterator = getIterator
    val collection = getNewCollection[U, W]
    while (iterator.hasNext) {
      val elem = iterator.next

      val innerIterator = fn(elem._1, elem._2)
      while (innerIterator.hasNext) {
        val innerElem = innerIterator.next
        collection.put(innerElem._1, innerElem._2)
      }
    }
    collection
  }

  def foldLeft[U](initial: U)(fn: (U, (T, V)) => U): U = {
    foldLeft0(getIterator, fn, initial)
  }

  def filter(pred: (T, V) => Boolean): CollectionType[T, V] = {
    val iterator = getIterator
    val collection = getNewCollection[T, V]
    while (iterator.hasNext) {
      val elem = iterator.next
      if (pred(elem._1, elem._2)) collection.put(elem._1, elem._2)
    }
    collection
  }

  def partition(pred: (T, V) => Boolean): (CollectionType[T, V], CollectionType[T, V]) = {
    val iterator = getIterator
    val satisfiesPredicate: CollectionType[T, V] = getNewCollection[T, V]
    val doesNotSatisfyPredicate = getNewCollection[T, V]

    while (iterator.hasNext) {
      val elem = iterator.next
      if (pred(elem._1, elem._2)) satisfiesPredicate.put(elem._1, elem._2)
      else doesNotSatisfyPredicate.put(elem._1, elem._2)
    }

    (satisfiesPredicate, doesNotSatisfyPredicate)
  }


  private def foldLeft0[U](iterator: java.util.Iterator[(T, V)], fn: (U, (T, V)) => U, accumulator: U): U = {
    if (iterator.hasNext)
      foldLeft0(iterator, fn, fn(accumulator, iterator.next))
    else
      accumulator
  }
}