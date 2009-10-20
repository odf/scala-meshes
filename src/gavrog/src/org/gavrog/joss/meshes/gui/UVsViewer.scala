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

import java.awt.Color

import de.jreality.geometry.{IndexedFaceSetFactory, IndexedLineSetFactory}
import de.jreality.math.MatrixBuilder
import de.jreality.scene.{DirectionalLight, SceneGraphComponent}
import de.jreality.scene.tool.{AbstractTool, InputSlot, ToolContext}
import de.jreality.shader.CommonAttributes._
import de.jreality.tools.ClickWheelCameraZoomTool
import de.jreality.util.SceneGraphUtility

import scala.swing.Dialog
import scala.swing.event.MouseEntered

import JRealitySupport._
import Sums._
import Vectors._

class UVsViewer extends JRealityViewerComponent
with KeyPublisher with KeyDispatcher
{
  implicit def xdouble(d: Double) = new { def deg = d / 180.0 * Math.Pi }
  implicit def xint(i: Int) = new { def deg = i / 180.0 * Math.Pi }
  implicit def wrapIter[T](it: java.util.Iterator[T]) = new Iterator[T] {
    def hasNext = it.hasNext
    def next = it.next
  }
  implicit def wrap[T](it: java.lang.Iterable[T]) = wrapIter(it.iterator)

  addTool(new PanTool)
  addTool(new ClickWheelCameraZoomTool)
    
  perspective = false
  var front_to_back = List[SceneGraphComponent]()
  var selection = Set[SceneGraphComponent]()
  var hidden = List[Set[SceneGraphComponent]]()
  var hot: SceneGraphComponent = null
    
  background_color = Color.LIGHT_GRAY
  size = (600, 800)
  setLight("Main Light",
           new DirectionalLight { setIntensity(1.0) },
           MatrixBuilder.euclidean)
  scene.addChild(grid)
  super.encompass
    
  def update_z_order = modify {
    for ((node, z) <- front_to_back.zipWithIndex)
      MatrixBuilder.euclidean.translate(0, 0, -0.01 * z).assignTo(node)
  }
    
  def setMesh(mesh: Mesh) = modify {
    SceneGraphUtility.removeChildren(scene)
    for (chart <- mesh.charts)
      scene.addChild(new UVsGeometry(chart, "uv-chart"))
    front_to_back = scene.getChildComponents.toList
    update_z_order
    scene.addChild(grid)
    encompass
  }
    
  override def encompass {
    grid.setVisible(false)
    super.encompass
    grid.setVisible(true)
  }
    
  def encompassSelected {
    var hidden = Set[SceneGraphComponent](grid)
    grid.setVisible(false)
    if (!selection.isEmpty)
      for (sgc <- scene.getChildComponents
           if (!selection.contains(sgc) && sgc.isVisible)) {
        hidden += sgc
        sgc.setVisible(false)
      }
    super.encompass
    for (sgc <- hidden) sgc.setVisible(true)
  }
    
  override def rotateScene(axis: Vec3, angle: Double) =
    super.rotateScene(new Vec3(0, 0, 1), angle)
  override def viewFrom(eye: Vec3, up: Vec3) =
    super.viewFrom(new Vec3(0, 0, 1), up)
    
  def set_color(sgc: SceneGraphComponent, c: Color) =
    sgc.getAppearance.setAttribute(POLYGON_SHADER + '.' + DIFFUSE_COLOR, c)
    
  def select(sgc: SceneGraphComponent) {
    set_color(sgc, Color.RED)
    selection += sgc
    clear_hot
  }
  def deselect(sgc: SceneGraphComponent) {
    set_color(sgc, Color.WHITE)
    selection -= sgc
    clear_hot
  }
  def clear_hot = if (hot != null) {
    set_color(hot, if (selection.contains(hot)) Color.RED else Color.WHITE)
    hot = null
  }
  def make_hot(sgc: SceneGraphComponent) = if (hot != sgc) {
    clear_hot
    set_color(sgc, if (selection.contains(sgc)) Color.YELLOW else Color.GREEN)
    hot = sgc
  }
  def hide(sgc: SceneGraphComponent) {
    sgc.setVisible(false)
    deselect(sgc)
  }
  def show(sgc: SceneGraphComponent) {
    sgc.setVisible(true)
  }
  def push_to_back(sgc: SceneGraphComponent) {
    front_to_back = (front_to_back - sgc) ::: List(sgc)
    update_z_order
  }
  def pull_to_front(sgc: SceneGraphComponent) {
    front_to_back = sgc :: (front_to_back - sgc)
    update_z_order
  }
  
  addTool(new AbstractTool {
    addCurrentSlot(InputSlot.getDevice("PointerTransformation")) // mouse move
      
    override def perform(tc: ToolContext) = modify {
      val pr = tc.getCurrentPick
      if (pr == null) clear_hot
      else make_hot(pr.getPickPath.getLastComponent)
    }
  })
    
  addTool(new AbstractTool(InputSlot.getDevice("PrimaryAction")) { // mouse 1
    override def activate(tc: ToolContext) {
      val pr = tc.getCurrentPick
      if (pr == null) return
      val sgc = pr.getPickPath.getLastComponent
      modify { if (selection contains sgc) deselect(sgc) else select(sgc) }
    }
  })
    
  addKeySource(this)
  focusOnEnter(this)
  enableContextMenu(this)
  
  bind("HOME", "reset view",
       modify {
         viewFrom(Vec3(0, 0, 1), Vec3(0, 1, 0))
         encompass
       })
  bind("0",     "zoom on selection ", modify { encompassSelected })
  bind("ctrl LEFT", "rotate counterclockwise",
       modify { rotateScene(Vec3(0, 0, 1), 5 deg) })
  bind("ctrl RIGHT", "rotate clockwise",
       modify { rotateScene(Vec3(0, 0, 1), -5 deg) })
  bind("SPACE", "deselect all",       modify { selection map deselect })
  bind("I",     "invert selection",
       modify {
         val new_selection = front_to_back.filter(!selection.contains(_))
         selection map deselect
         new_selection map select
       })
  bind("B", "push selected to back",  modify { selection map push_to_back })
  bind("F", "pull selected to front", modify { selection map pull_to_front })
  bind("H", "hide selected",
       if (selection.size > 0) modify {
         hidden = (Set() ++ selection) :: hidden
         selection map hide
       })
  bind("U", "unhide last hidden",
       hidden match {
         case last_batch :: rest => modify {
           last_batch map show
           hidden = rest
         }
         case Nil => ()
       })
  
  class UVsGeometry(chart: Mesh.Chart, name: String)
  extends SceneGraphComponent(name) {
    setGeometry(new IndexedFaceSetFactory {
      val vertices = chart.vertices.toArray
      var toNr = Map[Mesh.TextureVertex, Int]()
      for ((v, n) <- vertices.zipWithIndex) toNr += (v -> n)
      val faces = chart.faces.toArray
      setVertexCount(vertices.size)
      setFaceCount(faces.size)
      setVertexCoordinates(vertices.map(v => Array(v.x, v.y, 0)))
      setFaceIndices(faces.map(_.textureVertices.map(toNr).toArray))
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
  
  object grid extends SceneGraphComponent("Grid") {
    setGeometry(new IndexedLineSetFactory {
      setVertexCount(16)
      setLineCount(8)
      setVertexCoordinates(Array[Array[Double]](
        Array(-1, -1, 0), Array(-1,  0, 0), Array(-1,  1, 0), Array(-1,  2, 0),
        Array( 2, -1, 0), Array( 2,  0, 0), Array( 2,  1, 0), Array( 2,  2, 0),
        Array(-1, -1, 0), Array( 0, -1, 0), Array( 1, -1, 0), Array( 2, -1, 0),
        Array(-1,  2, 0), Array( 0,  2, 0), Array( 1,  2, 0), Array( 2,  2, 0)
      ))
      setEdgeIndices(Array(
        Array( 0,  4), Array( 1,  5), Array( 2,  6), Array( 3,  7),
        Array( 8, 12), Array( 9, 13), Array(10, 14), Array(11, 15)))
      update
    }.getIndexedLineSet)
    //setPickable(false)
    
    setAppearance(new RichAppearance(
      FACE_DRAW                                -> false,
      EDGE_DRAW                                -> true,
      TUBES_DRAW                               -> false,
      VERTEX_DRAW                              -> false,
      LINE_WIDTH                               -> 1.0,
      LINE_SHADER + '.' + DIFFUSE_COLOR        -> Color.BLUE,
      LINE_SHADER + '.' + SPECULAR_COEFFICIENT -> 0.0,
      PICKABLE                                 -> false
    ))
  }
}
