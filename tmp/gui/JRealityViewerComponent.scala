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

import java.awt.{Color, Dimension}
import java.awt.event.{KeyListener, MouseListener,
                       MouseMotionListener, MouseWheelListener}

import scala.swing.{BorderPanel, Component}

import de.jreality.geometry.GeometryUtility
import de.jreality.math.{Matrix, MatrixBuilder}
import de.jreality.scene.{Appearance, Camera, Light, SceneGraphComponent,
                          SceneGraphPath, Transformation, Viewer}
import de.jreality.scene.tool.Tool
import de.jreality.shader.CommonAttributes
import de.jreality.softviewer.SoftViewer
import de.jreality.toolsystem.ToolSystem
import de.jreality.util.{CameraUtility, RenderTrigger}

import JRealitySupport._
import SwingSupport._
import Vectors._

class JRealityViewerComponent(content: SceneGraphComponent) extends BorderPanel
{
  def this() = this(new SceneGraphComponent)
  
  private val sceneNode  = new SceneGraphComponent {
    addChild(content)
  }
  private val cameraNode = new SceneGraphComponent {
    setCamera(new Camera)
    setTransformation(MatrixBuilder.euclidean.translate(0, 0, 3))
  }
  private val lightNode  = new SceneGraphComponent
	
  private val rootNode = new SceneGraphComponent {
    addChild(sceneNode)
    addChild(cameraNode)
    addChild(lightNode)
    setAppearance(new RichAppearance(
      CommonAttributes.BACKGROUND_COLOR -> Color.DARK_GRAY,
      CommonAttributes.DIFFUSE_COLOR    -> Color.RED
    ))
  }

  private val renderTrigger = new RenderTrigger

  private val camPath =
    new SceneGraphPath(rootNode, cameraNode) { push(cameraNode.getCamera) }
  private val emptyPickPath = new SceneGraphPath(rootNode, sceneNode, content)

  private var lights                    = Map[String, SceneGraphComponent]()
  private var currentViewer: Viewer     = null
  private var lastCenter: Array[Double] = null

  private val softwareViewer = new SoftViewer { setupViewer(this) }

  viewer = try {
    new de.jreality.jogl.Viewer {
      def source = JRealityViewerComponent.this.peer
          
      def dispatchK(e: java.awt.event.KeyEvent) = source.dispatchEvent(
        new java.awt.event.KeyEvent(source, e.getID, e.getWhen, e.getModifiers,
                     e.getKeyCode, e.getKeyChar))
      def dispatchM(e: java.awt.event.MouseEvent) = source.dispatchEvent(
        new java.awt.event.MouseEvent(source, e.getID, e.getWhen, e.getModifiers,
                       e.getX, e.getY, e.getClickCount,
                       e.isPopupTrigger, e.getButton))
      def dispatchW(e: java.awt.event.MouseWheelEvent) = source.dispatchEvent(
        new java.awt.event.MouseWheelEvent(source, e.getID, e.getWhen, e.getModifiers,
                            e.getX, e.getY, e.getClickCount,
                            e.isPopupTrigger, e.getScrollType,
                            e.getScrollAmount, e.getWheelRotation))
          
      canvas.addKeyListener(new KeyListener {
        def keyPressed (e: java.awt.event.KeyEvent) = dispatchK(e)
        def keyReleased(e: java.awt.event.KeyEvent) = dispatchK(e)
        def keyTyped   (e: java.awt.event.KeyEvent) = dispatchK(e)
      })
      canvas.addMouseListener(new MouseListener {
        def mouseClicked (e: java.awt.event.MouseEvent) = dispatchM(e)
        def mouseEntered (e: java.awt.event.MouseEvent) = dispatchM(e)
        def mouseExited  (e: java.awt.event.MouseEvent) = dispatchM(e)
        def mousePressed (e: java.awt.event.MouseEvent) = dispatchM(e)
        def mouseReleased(e: java.awt.event.MouseEvent) = dispatchM(e)
      })
      canvas.addMouseMotionListener(new MouseMotionListener {
        def mouseDragged (e: java.awt.event.MouseEvent) = dispatchM(e)
        def mouseMoved   (e: java.awt.event.MouseEvent) = dispatchM(e)
      })
      canvas.addMouseWheelListener(new MouseWheelListener {
        def mouseWheelMoved(e: java.awt.event.MouseWheelEvent) = dispatchW(e)
      })
      
      setupViewer(this)
    }
  } catch {
    case ex: Exception => {
      System.err.println("OpenGL viewer could not be initialized.")
      softwareViewer
    }
  }
  
  size = (640, 400)
  
  if (viewer.isInstanceOf[de.jreality.jogl.Viewer]) invokeAndWait {
    try {
      viewer.asInstanceOf[de.jreality.jogl.Viewer].run
      System.err.println("OpenGL okay!")
    } catch {
      case ex: javax.media.opengl.GLException => {
        System.err.println("OpenGL viewer could not render.");
        viewer = softwareViewer
      }
      case ex: Exception => ex.printStackTrace
    }
  }

  fieldOfView = defaultFieldOfView
  renderTrigger.addSceneGraphComponent(rootNode)

  private def setupViewer(viewer: Viewer) {
    viewer.setSceneRoot(rootNode)
    viewer.setCameraPath(camPath)
    val ts = ToolSystem.toolSystemForViewer(viewer)
    ts.initializeSceneTools()
    ts.setEmptyPickPath(emptyPickPath)
  }
  
  def viewer = currentViewer
  
  def viewer_=(newViewer: Viewer) = invokeAndWait {
    val d = size
    renderTrigger.removeViewer(viewer)
    renderTrigger.addViewer(newViewer)
    currentViewer = newViewer
    add(new Component { override lazy val peer = viewingComponent },
        BorderPanel.Position.Center)
    size = d
  }

  private def viewingComponent =
    viewer.getViewingComponent.asInstanceOf[javax.swing.JComponent]
 
  override def size = if (currentViewer == null) new Dimension(0, 0)
                      else {
                        val d = currentViewer.getViewingComponentSize
                        new Dimension(d.width, d.height)
                      }
  
  override def size_=(d: Dimension) = invokeAndWait {
    viewingComponent.setPreferredSize(d)
    revalidate
  }
  override def size_=(d: (Int, Int)) = size_=(new Dimension(d._1, d._2))

  def background_color =
    rootNode.getAppearance.getAttribute(CommonAttributes.BACKGROUND_COLOR)
  
  def background_color_=(c: Color) =
    rootNode.getAppearance.setAttribute(CommonAttributes.BACKGROUND_COLOR, c)
  
  def setLight(name: String, light: Light, t: Transformation) {
    val node = lights.get(name) match {
      case Some(node) => node
      case None => {
        val node = new SceneGraphComponent
        lights(name) = node
        lightNode.addChild(node)
        node
      }
    }
    node.setLight(light)
    node.setTransformation(t)
  }
  
  def removeLight(name: String) {
    lights.get(name) match {
      case Some(node) => {
        lightNode.removeChild(node)
        lights -= name
      }
      case None => {}
    }
  }
 
  def addTool(t: Tool) = scene.addTool(t)
  
  def startRendering = renderTrigger.finishCollect
  def pauseRendering = renderTrigger.startCollect
  
  def modify(body: => Unit) {
    invokeAndWait { pauseRendering }
    new Thread {
      body
      invokeAndWait { startRendering }
    }.start
  }

  private def camera = camPath.getLastElement.asInstanceOf[Camera]
  def fieldOfView = camera.getFieldOfView
  def fieldOfView_=(x: Double) = camera.setFieldOfView(x)
  def defaultFieldOfView = 25.0
  
  def perspective = camera.isPerspective
  def perspective_=(value: Boolean) { camera.setPerspective(value) }
  
  def encompass {
    // -- extract parameters from scene and viewer
    val ts = ToolSystem.toolSystemForViewer(viewer)
    val avatarPath = ts.getAvatarPath
    val scenePath = ts.getEmptyPickPath
    val cameraPath = viewer.getCameraPath
    val aspectRatio = CameraUtility.getAspectRatio(viewer)

    // -- compute scene-to-avatar transformation
    val toAvatar = new Matrix
    scenePath.getMatrix(toAvatar.getArray, 0, scenePath.getLength - 2)
    toAvatar.multiplyOnRight(avatarPath.getInverseMatrix(null))

    // -- compute bounding box of scene
    val bounds = GeometryUtility.calculateBoundingBox(
      toAvatar.getArray, scenePath.getLastComponent)
    if (bounds.isEmpty) return

    // -- compute best camera position based on bounding box and viewport
	val vp = CameraUtility.getViewport(camera, aspectRatio)
	val e = bounds.getExtent
	val radius = Math.sqrt(e(0) * e(0) + e(2) * e(2) + e(1) * e(1)) / 2.0
    val front = e(2) / 2.0

    val xscale = e(0) / vp.getWidth
	val yscale = e(1) / vp.getHeight
	var camdist = Math.max(xscale, yscale) * 1.1
	if (!camera.isPerspective) {
	  camdist *= camera.getFocus // adjust for viewport scaling
      camera.setFocus(camdist)
	}

	// -- compute new camera position and adjust near/far clipping planes
    val c = bounds.getCenter
	c(2) += front + camdist
	camera.setFar(camdist + front + 5 * radius)
	camera.setNear(0.5 * camdist)

	// -- make sceneRotation recompute the center
	lastCenter = null

	// -- adjust the avatar position to make scene fit
	val camMatrix = new Matrix
	cameraPath.getInverseMatrix(camMatrix.getArray, avatarPath.getLength)
	val avatar = avatarPath.getLastComponent
	val mb = MatrixBuilder.euclidean(avatar)
    mb.translate(c).translate(camMatrix.getColumn(3)).assignTo(avatar)
  }
  
  def scene = emptyPickPath.getLastComponent
  
  def computeCenter = {
    val bounds = GeometryUtility.calculateBoundingBox(scene)
    if (bounds.isEmpty) Array(0.0, 0.0, 0.0, 1.0)
    else (new Matrix(scene.getTransformation).getInverse
            .multiplyVector(bounds.getCenter))
  }
  
  def rotateScene(axis: Vec3, angle: Double) {
    val m = new Matrix(scene.getTransformation)
    sceneRotation = MatrixBuilder.euclidean.rotate(angle, axis.toArray).times(m)
  }

  def viewFrom(eye: Vec3, up: Vec3) {
    var (u, v, w) = (up x eye, up, eye)
    u = u.unit
    v = (v - u * v * u).unit
    w = (w - u * w * u - v * w * v).unit
    sceneRotation = new Matrix(u.x, v.x, w.x, 0,
                               u.y, v.y, w.y, 0,
                               u.z, v.z, w.z, 0,
                                 0,   0,   0, 1 ).getInverse
  }
  
  def sceneRotation = new Matrix(content.getTransformation)
  
  def sceneRotation_=(tNew: Matrix) {
    if (lastCenter == null) lastCenter = computeCenter
    val center = lastCenter
    val tOld = sceneRotation
    val p = tOld.multiplyVector(center)
    val q = tNew.multiplyVector(center)
    val mb = MatrixBuilder.euclidean.translateFromTo(q, p).times(tNew)
    mb.assignTo(scene)
  }
  
  def sceneRotation_=(mb: MatrixBuilder) : Unit = sceneRotation_=(mb.getMatrix)
  def sceneRotation_=(t: Transformation) : Unit = sceneRotation_=(new Matrix(t))
  
  def screenshot(size: (Int, Int), antialias: Int, file: java.io.File) {
    import java.awt.{Graphics2D, Image, RenderingHints}
    import java.awt.image.BufferedImage
    import de.jreality.util.ImageUtility
    
    val (width, height) = size
    val img = softwareViewer.renderOffscreen(width * antialias,
                                             height * antialias)
    val scaledImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val gr = scaledImg.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                        RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    scaledImg.getGraphics.drawImage(
      img.getScaledInstance(width, height, Image.SCALE_SMOOTH), 0, 0, null)
    ImageUtility.writeBufferedImage(file, scaledImg)
  }

  def screenshot(size: (Int, Int), antialias: Int, file: String) {
    screenshot(size, antialias, new java.io.File(file))
  }
}
