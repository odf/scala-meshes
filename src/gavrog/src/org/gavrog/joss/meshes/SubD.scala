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

import scala.io.Source
import scala.swing.Reactor

object SubD extends Reactor {
  case class IteratedFunction[A](f: A => A, n: Int) {
    def ^(m: Int) = IteratedFunction(f, n * m)
    def apply(x: A) = {
      def iter(x: A, m: Int): A = if (m <= 0) x else iter(f(x), m-1)
      iter(x, n)
    }
  }
  implicit def toRichFun[A](f: A => A) = new IteratedFunction(f, 1)
    
  def main(args : Array[String]) : Unit = {
    val n = if (args.length > 1) args(1).toInt else 1
    
    System.err.println("Reading...")
    val src = if (args.length > 0) new Mesh(Source fromFile args(0))
              else new Mesh(System.in)

    listenTo(src)
    reactions += {
      case MessageSent(src, txt) => System.err.println(txt)
    }
    
    System.err.println("Processing...")
    val step: Mesh => Mesh = if (n > 0) _.subdivision else _.coarsening
    val dst = (step^(n abs))(src)

    System.err.println("Writing...")
    dst.write(System.out, "materials")

    System.err.println("Done.")
  }
}
