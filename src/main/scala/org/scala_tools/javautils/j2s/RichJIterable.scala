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

import java.lang.Iterable
import java.util.Iterator
import scala.{Iterable => SIterable}
import org.scala_tools.javautils.s2j.SIterableWrapper
import org.scala_tools.javautils.Implicits

class RichJIterable[T](iterable: Iterable[T]) extends HigherOrderCollectionFunctions[T, java.util.Collection] {
  override def getNewCollection[V] = {
    val iterableClass = iterable.getClass.asInstanceOf[Class[java.util.Collection[V]]]
    iterableClass.newInstance
  }
  
  override def getIterator: java.util.Iterator[T] = iterable.iterator

  def asScala: SIterable[T] = iterable match {
    case iw: SIterableWrapper[_] =>
      iw.asScala.asInstanceOf[SIterable[T]]
    case _ => new JIterableWrapper[T] {
      type Wrapped = Iterable[T]
      val underlying = iterable
    }
  }
}
