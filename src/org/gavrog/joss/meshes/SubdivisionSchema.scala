package org.gavrog.joss.meshes

import Sums._
import Vectors._

class SubdivisionSchema(base: Mesh) {
    // -- create a new empty mesh
    val subD = new Mesh
    
    // -- copy the original vertices, texture vertices and normals
    for (v <- base.vertices) subD.addVertex(v.pos)
    for (t <- base.textureVertices) subD.addTextureVertex(t.pos)
    for (n <- base.normals) subD.addNormal(n.value)
    
    // -- create edge centers
    val ch2ev = Map[Mesh.Chamber, Mesh.Vertex]() ++ (
      for { e <- base.edges
            c = e.from.chamber
            z = subD.addVertex((c.start.pos + c.end.pos) / 2)
            d <- List(c, c.s0, c.s2, c.s0.s2).elements }
      	yield (d -> z)
    )
    
    // -- interpolate texture coordinates along vertices
    var ch2etnr = Map[Mesh.Chamber, Int]()
    for (c <- base.chambers if !ch2etnr.contains(c)) {
      val t1 = c.tVertex
      val t2 = c.s0.tVertex
      if (t1 != null && t2 != null) {
        val z = subD.addTextureVertex((t1.pos + t2.pos) / 2)
        for (d <- List(c, c.s0)) ch2etnr += (d -> z.nr)
      }
    }
    
    // -- create face centers and subdivision faces
    for (f <- base.faces) {
      val n = f.degree
      val z = subD.addVertex(f.vertices.sum(_.pos) / n).nr
      val tz =
        if (f.textureVertices.forall(null !=))
          subD.addTextureVertex(f.textureVertices.sum(_.pos) / n).nr
        else 0
      for (c <- f.vertexChambers) {
        val t = c.tVertexNr
        val n = c.normalNr
        val g = subD.addFace(
          List(c.vertexNr,  ch2ev(c).nr, z, ch2ev(c.s1).nr),
          List(t, ch2etnr.getOrElse(c, 0), tz, ch2etnr.getOrElse(c.s1, 0)),
          List(n, n, n, n))
        g.obj      = subD.obj(f.obj.name)
        g.material = subD.material(f.material.name)
        g.group    = subD.group(f.group.name)
        g.smoothingGroup = f.smoothingGroup
      }
    }
    
    // -- fill holes and flag border vertices
    subD.fixHoles
    var onBorder = Set[Mesh.Vertex]()
    for (c <- subD.hardChambers) onBorder += c.vertex
    
    // -- adjust positions of edge centers
    for (e <- base.edges; val z = ch2ev(e.from.chamber) if !onBorder(z)) {
      if (z.degree != 4) error("bad new vertex degree in subdivision()")
      z.pos = z.cellChambers.sum(_.s0.vertex.pos) / 4
    }
    
    // -- adjust positions of (copied) original non-border vertices
    for (n <- 1 to base.numberOfVertices;
         val v = subD.vertex(n) if !onBorder(v)) {
      val k = v.degree
      val cs = v.cellChambers.toSeq
      v.pos = (((k - 3) * v.pos
                + 4 * cs.sum(_.s0.vertex.pos) / k
                - cs.sum(_.s0.s1.s0.vertex.pos) / k) / k)
    }
    
    // -- do the same for border vertices
    val hard = Set[Mesh.Chamber]() ++ subD.hardChambers
    for (v <- onBorder if v.nr <= base.numberOfVertices) {
      val breaks = v.cellChambers.filter(hard).toSeq
      if (breaks.size == 2)
        v.pos =
          (breaks(0).s0.vertex.pos + breaks(1).s0.vertex.pos + 2 * v.pos) / 4
    }
    
    // -- return the result
    subD.mtllib ++ base.mtllib
    subD
}
