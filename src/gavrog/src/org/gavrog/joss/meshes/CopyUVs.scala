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

object CopyUVs {
  def bail(message: String) {
    System.err.println(message)
    exit(1)
  }
  
  def main(args: Array[String]) : Unit = {
    var i = 0

    if (args.size < i + 2) bail("need two .obj files as arguments")
    val mesh  = new Mesh(Source.fromFile(args(i)))
    val donor = new Mesh(Source.fromFile(args(i + 1)))

    val originals = mesh.charts
    
    for (chart <- donor.charts; c <- originals) {
      val map = Mesh.closest(Mesh.allMatches(chart, c), _.tVertex)
      if (map != null) {
        System.err.println(
          "Transferring data for chart with %d chambers."
          format (chart.chambers.size))
        for ((c, d) <- map) d.tVertex.pos = c.tVertex.pos
      }
    }
    mesh.write(System.out, "materials")
  }
}
