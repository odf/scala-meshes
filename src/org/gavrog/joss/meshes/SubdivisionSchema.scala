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

class SubdivisionSchema(base: Mesh) {
    // -- creates a sparse vector for a given vertex encoding it as a variable
	private def variable(v: Mesh.Vertex) = new SparseVector(v.nr -> 1.0)

	// -- initialize a mapping from subdivision mesh indices to weight vectors
	private var weights = Map[Int, SparseVector]()
 
	// -- computes the average weight vector for a sequence of vertices
    private def avgWeight(s: Seq[Mesh.Vertex]) =
      s.map(_.nr).map(weights).reduceLeft(_+_) / s.length
 
    // -- create a new empty mesh
    val mesh = new Mesh
    
    // -- creates a new vertex
    private def newVertex = mesh.addVertex(0, 0, 0)
    
	// -- make a copy of each vertex in the original mesh
    for (v <- base.vertices) weights += (newVertex.nr -> variable(v))

    // -- create edge centers
    private val ch2ev = Map() ++ (
      for {
    	c <- base.edgeChambers
    	z = newVertex
    	d <- List(c, c.s0, c.s2, c.s0.s2).elements
      }
      	yield (d -> z)
    )
    
    // -- create face centers and subdivision faces
    for (f <- base.faces) {
      val z = newVertex.nr
      weights += (z -> avgWeight(f.vertices))
      for (c <- f.vertexChambers) {
        val g = mesh.addFace(List(c.vertexNr,  ch2ev(c).nr, z, ch2ev(c.s1).nr),
          List(0, 0, 0, 0), List(0, 0, 0, 0))
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
      val test = if (onBorder(z)) hard else (c: Any) => true
      val verts = z.cellChambers.filter(test).map(_.s0.vertex)
      weights += (z.nr -> avgWeight(verts))
    }
    
    // -- adjust positions of (copied) original vertices
    for (v <- 1 to base.numberOfVertices map mesh.vertex)
      if (onBorder(v)) {
    	val breaks = v.cellChambers.filter(hard).toSeq.map(_.s0.vertex)
    	if (breaks.size == 2)
    	  weights += (v.nr -> avgWeight(breaks ++ List(v, v)))
      } else {
    	val k = v.degree
    	val cs = v.cellChambers.toSeq
    	weights += (v.nr -> (weights(v.nr) * (k - 3)
                             + avgWeight(cs.map(_.s0.vertex)) * 4
                             - avgWeight(cs.map(_.s0.s1.s0.vertex))) / k)
      }
    
    // -- update the materials
    mesh.mtllib ++ base.mtllib
    
    // -- set vertex positions based on weights
    for (v <- mesh.vertices) v.pos =
      weights_for(v).map(e => base.vertex(e._1).pos * e._2).reduceLeft(_+_)
   
    def weights_for(v: Mesh.Vertex) = weights(v.nr).items
}

object ComputeTransferWeights {
  def verticesByGroup(m: Mesh) = Map() ++ m.groups.map(g =>
    (g.name, {
      val faces = m.faces.filter(f => f.group == g).toList
      val verts = Set() ++ faces.flatMap(_.vertices)
      m.vertices.filter(null !=).filter(verts contains).toList
    }))
  
  def main(args : Array[String]) : Unit = {
    import java.io.OutputStreamWriter
    import scala.io.Source
    
    val (split, i) = if (args(0) == "-s") (true, 1) else (false, 0)

    System.err.println("Reading low-poly source mesh...")
    val src = new Mesh(Source fromFile args(i))
    System.err.println("Reading high-poly transfer target...")
    val dst = new Mesh(Source fromFile args(i+1))
    
    System.err.println("Subdividing source to compute weights...")
    val sub = new SubdivisionSchema(src)

    System.err.println("Matching result to transfer target...")
    val map = sub.mesh.findMapping(dst)
    
    System.err.println("Writing weights...")
    val writer = new OutputStreamWriter(System.out)
    
    def writeWeights(n: Int, v: Mesh.Vertex) {
      val weights = sub.weights_for(map(v.chamber).vertex).toList.sort(_<_)
      writer.write("w %d %d" format (n, weights.length))
      for ((n, f) <- weights) writer.write(" %d" format (n - 1))
      for ((n, f) <- weights) writer.write(" %.8f" format f)
      writer.write("\n")
    }
    
    if (split)
      for ((name, verts) <- verticesByGroup(dst)) {
        writer.write("actor %s\n" format (name))
        for (n <- 0 to verts.size - 1) writeWeights(n, verts(n))
      }
    else
      for (v <- dst.vertices) writeWeights(v.nr - 1, v)
    
    writer.close
    
    System.err.println("Done.")
  }
}
