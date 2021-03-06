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

import java.util.Collection
import scala.{Iterable => SIterable, Collection => SCollection}
import org.scala_tools.javautils.s2j.SCollectionWrapper

class RichJCollection[T](collection: Collection[T]) extends HigherOrderCollectionFunctions[T, java.util.Collection] {
  override def getNewCollection[V] = {
    val collectionClass = collection.getClass.asInstanceOf[Class[Collection[V]]]
    collectionClass.newInstance
  }
  override def getIterator: java.util.Iterator[T] = collection.iterator
  
  def asScala: SCollection[T] = collection match {
    case cw: SCollectionWrapper[_] =>
      cw.asScala.asInstanceOf[SCollection[T]]
    case _ => new JCollectionWrapper[T] {
      type Wrapped = Collection[T]
      val underlying = collection
    }
  }
}
