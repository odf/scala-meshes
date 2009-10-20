/*
   Copyright 2009 Olaf Delgado-Friedrichs

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

object Sums {
  trait SemiGroup[A] {
    def add(x: A, y: A) : A
  }
  trait Monoid[A] extends SemiGroup[A] {
    def unit : A
  }
    
  implicit object IntMonoid extends Monoid[Int] {
    def add(x: Int, y: Int) = x + y
    def unit = 0
  }
    
  implicit object DoubleMonoid extends Monoid[Double] {
    def add(x: Double, y: Double) = x + y
    def unit = 0.0
  }

  trait Summable[T] {
    def foreach(f: T => Unit): Unit
  
    def sum[A](f: T => A)(implicit m : Monoid[A]) : A = {
      var s = m.unit;
      for (x <- this) s = m.add(s, f(x));
      s
    }
    def sum()(implicit m : Monoid[T]) : T = {
      var s = m.unit;
      for (x <- this) s = m.add(s, x);
      s
    }
    def count(f: T => Boolean) = sum(x => if (f(x)) 1 else 0)
  }
    
  implicit def wrapIterable[T](iter : Iterable[T]) = new Summable[T] {
    def foreach(f : T => Unit) = iter.foreach(f)
  }
  implicit def wrapIterator[T](iter : Iterator[T]) = new Summable[T] {
    def foreach(f : T => Unit) = iter.foreach(f)
  }
}
