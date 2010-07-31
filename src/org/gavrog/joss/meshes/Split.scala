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

import java.io.FileWriter
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Split {
  def main(args : Array[String]) {
	if (args.size > 1) {
	  val m = join(new Mesh(Source.fromFile(args(0))).splitByGroup)
	  m.write(new FileWriter(args(1)), null)	}
	else {
	  for (m <- new Mesh(Source.fromFile(args(0))).splitByGroup) {
		val name = m.groups.head.name
		m.write(new FileWriter("%s.obj" format name), name)
	  }
	}
  }
  
  def join(parts: Iterable[Mesh]) = {
	val m = new Mesh()
	var vMap = Map[(Mesh, Int), Int]()
	var tMap = Map[(Mesh, Int), Int]()
	var nMap = Map[(Mesh, Int), Int]()
    for (part <- parts if part.numberOfFaces > 0) {
      vMap ++= part.vertices.map(v => ((part, v.nr), m.addVertex(v.pos).nr))
      tMap ++= part.textureVertices.map(t =>
      	((part, t.nr), m.addTextureVertex(t.pos).nr))
      nMap ++= part.normals.map(n => ((part, n.nr), m.addNormal(n.value).nr))
    }
	for (part <- parts; f <- part.faces) {
	  val cs = ArrayBuffer() ++ f.vertexChambers
	  val vs = cs.map(c => vMap((part, c.vertexNr)))
	  val vt = cs.map(c => tMap((part, c.tVertexNr)))
	  val vn = cs.map(c => nMap((part, c.normalNr)))
	  val g = m.addFace(vs, vt, vn)
	  g.material = m.material(f.material.name)
	  g.obj = m.obj(f.obj.name)
	  g.group = m.group(f.group.name)
	  g.smoothingGroup = f.smoothingGroup
	}
	m
  }
}
