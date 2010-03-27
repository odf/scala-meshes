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

import Sums._
import Vectors._

class SubdivisionSchema(base: Mesh) {
    private implicit object SVec3Monoid extends Monoid[SparseVector] {
      def add(x: SparseVector, y: SparseVector) = x + y
      def unit = new SparseVector()
    }

    // -- create a sparse vector for each vertex encoding it as a variable
	private val variables =
	  Map() ++ base.vertices.map(v => (v -> new SparseVector(v.nr -> 1.0)))

	// -- initialize a mapping from subdivision mesh indices to weight vectors
	private var weights = Map[Int, SparseVector]()
 
    // -- create a new empty mesh
    val mesh = new Mesh
    
    // -- copy the original vertices, texture vertices and normals
    for (v <- base.vertices) {
      val w = mesh.addVertex(v.pos)
      weights += (w.nr -> variables(v))
    }
    
    // -- create edge centers
    private val ch2ev = Map() ++ (
      for {
    	c <- base.edgeChambers
    	z = mesh.addVertex((c.start.pos + c.end.pos) / 2)
    	d <- List(c, c.s0, c.s2, c.s0.s2).elements
      }
      	yield (d -> z)
    )
    
    // -- create face centers and subdivision faces
    for (f <- base.faces) {
      val n = f.degree
      val z = mesh.addVertex(f.vertices.sum(_.pos) / n).nr
      weights += (z -> f.vertices.sum(variables) / n)
      for (c <- f.vertexChambers) {
        val g = mesh.addFace(
          List(c.vertexNr,  ch2ev(c).nr, z, ch2ev(c.s1).nr),
          List(0, 0, 0, 0),
          List(0, 0, 0, 0))
        g.obj      = mesh.obj(f.obj.name)
        g.material = mesh.material(f.material.name)
        g.group    = mesh.group(f.group.name)
        g.smoothingGroup = f.smoothingGroup
      }
    }
    
    // -- fill holes and flag border vertices
    mesh.fixHoles
    private val hard = Set() ++ mesh.hardChambers
    private val onBorder = Set() ++ hard.map(_.vertex)
    
    // -- adjust positions of edge centers
    for (z <- base.edgeChambers.map(ch2ev)) {
      val verts = if (onBorder(z))
    	z.cellChambers.filter(hard).map(_.s0.vertex)
      else
        z.cellChambers.map(_.s0.vertex)
      weights += (z.nr -> verts.sum(v => weights(v.nr)) / verts.length)
    }
    
    // -- adjust positions of (copied) original vertices
    private def w0(c: Mesh.Chamber) = c.s0.vertex
    private def w1(c: Mesh.Chamber) = c.s0.s1.s0.vertex

    for (v <- 1 to base.numberOfVertices map mesh.vertex)
      if (onBorder(v)) {
    	val breaks = v.cellChambers.filter(hard).toSeq.map(_.s0.vertex)
    	if (breaks.size == 2)
    	  weights += (v.nr -> (weights(breaks(0).nr) + weights(breaks(1).nr)
                               + weights(v.nr) * 2) / 4)
      } else {
    	val k = v.degree
    	val cs = v.cellChambers.toSeq
    	weights += (v.nr -> (weights(v.nr) * (k - 3)
                             + cs.sum(v => weights(w0(v).nr)) * 4 / k
                             - cs.sum(v => weights(w1(v).nr)) / k) / k)
      }
    
    // -- update the materials
    mesh.mtllib ++ base.mtllib
    
    // -- set vertex positions based on weights
    for (v <- mesh.vertices)
      v.pos = weights_for(v).map(e => base.vertex(e._1).pos * e._2).sum
   
    def weights_for(v: Mesh.Vertex) = weights(v.nr).items
}

object Subdivision {
  def main(args : Array[String]) : Unit = {
    import java.io.FileWriter
    
    System.err.println("Reading input mesh...")
    val src = new Mesh(System.in)

    System.err.println("Processing...")
    val sub = new SubdivisionSchema(src)

    System.err.println("Writing output mesh...")
    sub.mesh.write(System.out, "materials")

    System.err.println("Writing weights...")
    val writer = new FileWriter("weights.txt")
    for (w <- sub.mesh.vertices) {
      val weights = sub.weights_for(w).toList
      writer.write("w %d %d" format (w.nr - 1, weights.length))
      for ((n, f) <- weights) writer.write(" %d" format (n - 1))
      for ((n, f) <- weights) writer.write(" %.8f" format f)
      writer.write("\n")
    }
    writer.close
    
    System.err.println("Done.")
  }
}
