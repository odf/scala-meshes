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

import scala.util.Sorting._
import scala.io.Source
import Sums._

object Info {
  def main(args : Array[String]) : Unit = {
    val mesh = if (args.length > 0) new Mesh(Source fromFile args(0))
               else new Mesh(System.in)

    val parts = mesh.components.toList.sort(_.chambers.size < _.chambers.size)
    println
    println("  %5d components"    format parts.size)
    for (p <- parts) {
      print("       %5d chambers" format p.chambers.size)
      print(", %3d symmetries"    format Mesh.allMatches(p, p).size)
      val c = p.coarseningClassifications
      print(", %1d coarsenings"   format c.size)
      print(" (%1d strict)"       format c.count(_.isStrict))
      println
    }
    val charts = mesh.charts.toList.sort(_.chambers.size < _.chambers.size)
    println("  %5d charts"        format charts.size)
    for (p <- charts) {
      print("       %5d chambers" format p.chambers.size)
      print(", %3d symmetries"    format Mesh.allMatches(p, p).size)
      println
    }
    println("  %5d vertices"          format mesh.numberOfVertices)
    println("  %5d edges"             format mesh.numberOfEdges)
    println("  %5d faces"             format mesh.numberOfFaces)
    println("  %5d holes"             format mesh.numberOfHoles)
    println("  %5d border edges"      format mesh.holes.sum(_.degree))
    println("  %5d normals"           format mesh.numberOfNormals)
    println("  %5d texture vertices"  format mesh.numberOfTextureVertices)
    println("  %5d groups"            format mesh.numberOfGroups)
    println("  %5d materials"         format mesh.numberOfMaterials)
  
    println
    println("  First vertex:         " + mesh.vertices.next)
    println("  First texture vertex: " + mesh.textureVertices.next)
    println("  First normal:         " + mesh.normals.next)
    println("  First group:          " + mesh.groups.next)
    println("  First material:       " + mesh.materials.next)
    println("  First face:           " + mesh.faces.next)
    
    println
    println("  Poles:")
    val vds = mesh.vertices.map(_.degree).toList
    println("    %5d isolated vertices"            format vds.count(0 ==))
    println("    %5d vertices of degree 1"         format vds.count(1 ==))
    println("    %5d vertices of degree 2"         format vds.count(2 ==))
    println("    %5d vertices of degree 3"         format vds.count(3 ==))
    println("    %5d vertices of degree 5"         format vds.count(5 ==))
    println("    %5d vertices of degree 6 or more" format vds.count(5 <))
    println("    %5d faces of degree 3 or less" format
                  mesh.faces.count(_.degree < 3))
    println("    %5d faces of degree 5 or more" format
                  mesh.faces.count(_.degree > 5))
  }
}
