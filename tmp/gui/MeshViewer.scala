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


package org.gavrog.joss.meshes.gui

import java.awt.{Color, Point}

import de.jreality.geometry.IndexedFaceSetFactory
import de.jreality.math.MatrixBuilder
import de.jreality.scene.{DirectionalLight, SceneGraphComponent}
import de.jreality.tools.ClickWheelCameraZoomTool
import de.jreality.util.SceneGraphUtility

import JRealitySupport._
import Sums._
import Vectors._

class MeshViewer extends JRealityViewerComponent
with KeyPublisher with KeyDispatcher
{
  implicit def xdouble(d: Double) = new { def deg = d / 180.0 * Math.Pi }
  implicit def xint(i: Int) = new { def deg = i / 180.0 * Math.Pi }
  implicit def asArray[A](it: Iterator[A]) = it.toList.toArray
  implicit def asArray[A](it: Iterable[A]) = it.toList.toArray
  
  addTool(new RotateTool(this))
  addTool(new PanTool)
  addTool(new ClickWheelCameraZoomTool)
    
  size = (600, 800)
  setLight("Main Light", new DirectionalLight { setIntensity(0.8) },
           MatrixBuilder.euclidean.rotateX(-30 deg).rotateY(-30 deg))
  setLight("Fill Light", new DirectionalLight { setIntensity(0.2) },
           MatrixBuilder.euclidean.rotateX(10 deg).rotateY(20 deg))

  var center = Array(0.0, 0.0, 0.0, 1.0)
  override def computeCenter = center

  def setMesh(mesh: Mesh) = modify {
    SceneGraphUtility.removeChildren(scene)
    scene.addChild(new MeshGeometry(mesh))
    center = (mesh.vertices.sum(_.pos) / mesh.numberOfVertices).toArray
    encompass
  }

  addKeySource(this)
  focusOnEnter(this)
  enableContextMenu(this)
  
  bind("HOME", "reset view", modify {
    viewFrom(Vec3(0, 0, 1), Vec3(0, 1, 0))
    fieldOfView = defaultFieldOfView
    encompass
  })
  bind("0", "zoom on scene", modify {
    fieldOfView = defaultFieldOfView
    encompass
  })
  
  bind("ctrl LEFT" , "rotate counterclockwise",
       modify { rotateScene(Vec3(0, 0, 1), +5 deg) })
  bind("ctrl RIGHT", "rotate clockwise",
       modify { rotateScene(Vec3(0, 0, 1), -5 deg) })
  bind("LEFT"      , "rotate left",
       modify { rotateScene(Vec3(0, 1, 0), -5 deg) })
  bind("RIGHT"     , "rotate right",
       modify { rotateScene(Vec3(0, 1, 0), +5 deg) })
  bind("UP"        , "rotate up",
       modify { rotateScene(Vec3(1, 0, 0), -5 deg) })
  bind("DOWN"      , "rotate down",
       modify { rotateScene(Vec3(1, 0, 0), +5 deg) })

  bind("X", "view from +x",
       modify { viewFrom(Vec3( 1, 0, 0), Vec3(0, 1, 0)) })
  bind("shift X", "view from -x",
       modify { viewFrom(Vec3(-1, 0, 0), Vec3(0, 1, 0)) })
  bind("Y", "view from y",
       modify { viewFrom(Vec3( 0, 1, 0), Vec3(0, 0,-1)) })
  bind("shift Y", "view from -y",
       modify { viewFrom(Vec3( 0,-1, 0), Vec3(0, 0, 1)) })
  bind("Z", "view from z",
       modify { viewFrom(Vec3( 0, 0, 1), Vec3(0, 1, 0)) })
  bind("shift Z", "view from -z",
       modify { viewFrom(Vec3( 0, 0,-1), Vec3(0, 1, 0)) })
  
  class MeshGeometry(mesh: Mesh) extends SceneGraphComponent {
    import de.jreality.shader.CommonAttributes._
    
    setGeometry(new IndexedFaceSetFactory {
      setVertexCount(mesh.numberOfVertices)
      setFaceCount(mesh.numberOfFaces)
      setVertexCoordinates(mesh.vertices.map(_.pos.toArray))
      setFaceIndices(mesh.faces.map(_.vertices.map(_.nr-1).toArray))
      setGenerateEdgesFromFaces(true)
      setGenerateFaceNormals(true)
      setGenerateVertexNormals(true)
      update
    }.getIndexedFaceSet)
    
    setAppearance(new RichAppearance(
      EDGE_DRAW                                   -> true,
      TUBES_DRAW                                  -> false,
      VERTEX_DRAW                                 -> false,
      FACE_DRAW                                   -> true,
      POLYGON_SHADER + '.' + DIFFUSE_COLOR        -> Color.WHITE,
      POLYGON_SHADER + '.' + SPECULAR_COEFFICIENT -> 0.1,
      SMOOTH_SHADING                              -> false,
      DEPTH_FUDGE_FACTOR                          -> 0.9999,
      LINE_WIDTH                                  -> 1.0,
      LINE_SHADER + '.' + DIFFUSE_COLOR           -> new Color(0.1f, 0.1f, 0.1f),
      LINE_SHADER + '.' + SPECULAR_COEFFICIENT    -> 0.0,
      // the following make sure no imaginary tubes and sphere are picked
      TUBE_RADIUS                                 -> 0.00001,
      POINT_RADIUS                                -> 0.00001
    ))
  }
}
