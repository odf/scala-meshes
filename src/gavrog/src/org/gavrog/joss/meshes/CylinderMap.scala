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

import collection.mutable.Queue
import io.Source

import Math.{abs, atan2, Pi}
import System.{in, out, err}

import Mesh.{Chamber, Face, TextureVertex}
import Vectors.{Vec2, Vec3}

object CylinderMap {
  def main(args : Array[String]) : Unit = {
    err.println("Reading...")
    val src = new Mesh(Source.fromFile(args(0)))
    
    err.println("Processing...")
    src.computeNormals
    val dst = new Mesh
    val vMap = new LazyMap((t: TextureVertex) => dst.addVertex(t.x, t.y, 0.0))
    val tMap = new LazyMap((t: TextureVertex) => {
      val v = t.chamber.vertex
      val n = t.chamber.normal
      dst.addTextureVertex(atan2(n.z, n.x)/ 4 / Pi + 0.5, v.y)
    })

    for (f <- src.faces) {
      val cs = f.textureVertices.toSeq
      val vs = cs.map(vMap(_).nr)
      val vt = cs.map(tMap(_).nr)
      val vn = cs.map(v => 0)
      val g = dst.addFace(vs, vt, vn)
      g.obj            = dst.obj("default")
      g.group          = dst.group(f.group.name)
      g.material       = dst.material(f.material.name)
      g.smoothingGroup = f.smoothingGroup
    }
    dst.fixHoles
    
    var seen = Set[Chamber]()
    for (c <- dst.chambers if !seen(c) && c.tVertex != null) {
      val queue  = new Queue[Chamber]
      def store(c: Chamber) = if (!seen(c)) { queue += c; seen += c }
      store(c)
      
      while (queue.length > 0) {
        val d = queue.dequeue
        val t1 = d.tVertex
        if (t1 != null && d.cell.isInstanceOf[Face]) {
          store(d.s0)
          store(d.s1)
          val t2 = d.s0.tVertex
          val u1 = t1.pos.x
          var u2 = t2.pos.x
          if (abs(u1 - (u2 + 0.5)) < abs(u1 - u2)) u2 += 0.5
          if (abs(u1 - (u2 - 0.5)) < abs(u1 - u2)) u2 -= 0.5
          t2.pos = new Vec2(u2, t2.pos.y)
          if (d.s2.tVertex == t1) store(d.s2)
        }
      }
    }
    
    err.println("Writing...")
    dst.write(out, "materials")
    err.println("Done.")
  }
}
