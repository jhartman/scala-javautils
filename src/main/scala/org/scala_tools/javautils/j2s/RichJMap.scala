/**
 * Copyright 2009 Jorge Ortiz
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

import scala.collection.{Map => SMap}
import scala.collection.mutable.{Map => SMutableMap}
import org.scala_tools.javautils.s2j.{SMapWrapper, SMutableMapWrapper}
import java.util.{Collection, HashMap, Map, Iterator}
import org.scala_tools.javautils.Implicits._



class RichJMap[K, V](map: Map[K, V]) extends MapHigherOrderFunctions[K, V, Map] {
  def getNewCollection[U, W]: Map[U, W] = {
    val mapClass = map.getClass.asInstanceOf[Class[Map[U, W]]]
    mapClass.newInstance
  }

  def getIterator: Iterator[(K, V)] = {
    val entrySetIterator = map.entrySet.iterator
    new Iterator[(K, V)] {
      def hasNext: Boolean = entrySetIterator.hasNext

      def next: (K, V) = {
        val entry = entrySetIterator.next
        (entry.getKey, entry.getValue)
      }

      def remove: Unit = throw new UnsupportedOperationException
    }
  }

  def +(kv: (K, V)): Map[K, V] = {
    map.put(kv._1, kv._2)
    map
  }

  // Scala iterable
  def ++(kvs: Iterable[(K, V)]): Map[K, V] = {
    kvs.foreach(kv => map.put(kv._1, kv._2))
    map
  }

  //Java iterable
  def ++(kvs: java.lang.Iterable[(K, V)]): Map[K, V] = {
    ++(kvs.asScala)
  }

  def ++(kvs: Array[(K, V)]): Map[K, V] = {
    ++(kvs.asJava)
  }

  def asScala: SMap[K, V] = map match {
    case mw: SMapWrapper[_, _] =>
      mw.asScala.asInstanceOf[SMap[K, V]]
    case _ => new JMapWrapper[K, V] {
      type Wrapped = Map[K, V]
      val underlying = RichJMap.this.map
    }
  }

  def asScalaMutable: SMutableMap[K, V] = map match {
    case mmw: SMutableMapWrapper[_, _] =>
      mmw.asScala.asInstanceOf[SMutableMap[K, V]]
    case _ => new JMutableMapWrapper[K, V] {
      type Wrapped = Map[K, V]
      val underlying = RichJMap.this.map
    }
  }
}
