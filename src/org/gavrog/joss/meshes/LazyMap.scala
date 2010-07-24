/*
   Copyright 2010 Olaf Delgado-Friedrichs

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/


package org.gavrog.joss.meshes

import scala.collection.mutable.HashMap

class LazyMap[A, B](f: A => B) extends HashMap[A, B] with Proxy {
  private val cache = new HashMap[A, B]
    
  def self               = cache
  override def size      = cache.size
  override def elements  = cache.iterator
  override def iterator  = cache.iterator
  override def get(x: A) = Some(cache.getOrElseUpdate(x, f(x)))
}
  
