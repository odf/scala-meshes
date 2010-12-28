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

import scala.io.Source
import scala.swing.Reactor
import java.io.FileWriter

object SplitMorph extends Reactor {
  def bail(message: String) {
    System.err.println(message)
    exit(1)
  }

  def main(args: Array[String]) : Unit = {
    if (args.size < 3) bail("need two .obj files as arguments and a threshold")

    val original  = new Mesh(Source fromFile args(0))
    val morphed   = new Mesh(Source fromFile args(1))
    val threshold = args(2).toDouble

    listenTo(original)
    reactions += {
      case MessageSent(src, txt) => System.err.println(txt)
    }

    val left = original.clone
    val right = original.clone
    for (v <- original.vertices) {
      val delta = morphed.vertex(v.nr).pos - original.vertex(v.nr).pos
      if (v.pos.x < -threshold) {
        right.vertex(v.nr).pos += delta
      }
      else if (v.pos.x < threshold) {
        right.vertex(v.nr).pos += delta * (threshold - v.pos.x) / (2 * threshold)
        left.vertex(v.nr).pos  += delta * (threshold + v.pos.x) / (2 * threshold)
      } else {
        left.vertex(v.nr).pos += delta
      }
    }
    left.write(new FileWriter(args(1).replace(".obj", "Left.obj")), "materials")
    right.write(new FileWriter(args(1).replace(".obj", "Right.obj")), "materials")
  }
}
