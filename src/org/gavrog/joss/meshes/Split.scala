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

import java.io.FileWriter
import scala.io.Source

object Split {
  def main(args : Array[String]) : Unit =
    for (m <- new Mesh(Source.fromFile(args(0))).splitByGroup) {
      val name = m.groups.next.name
      m.write(new FileWriter("%s.obj" format name), name)
    }
}
