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
import java.io.{File, FileWriter}

import scala.swing.{Action, BorderPanel, MainFrame, Menu, MenuBar,
                    MenuItem, Orientation, Separator, SplitPane, TextArea}

import SwingSupport._
import actions._

object View {
  val statusLine  = new TextArea(1, 80) { editable = false }
  val sceneViewer = new MeshViewer
  val uvMapViewer = new UVsViewer
  
  def log(message: String) = invokeAndWait {
    statusLine.text = message
  }
  
  def main(args : Array[String]) {
    new MainFrame {
      title = "Scala Mesh Viewer"
      val main = new BorderPanel {
        add(new SplitPane(Orientation.Vertical, sceneViewer, uvMapViewer) {
          continuousLayout = true
        }, BorderPanel.Position.Center)
        add(statusLine, BorderPanel.Position.South)
      }
      
      var _mesh: Mesh = null
      def mesh = _mesh
      def mesh_=(new_mesh: Mesh) {
        if (_mesh != null) deafTo(_mesh)
        _mesh = new_mesh
        sceneViewer.setMesh(_mesh)
        uvMapViewer.setMesh(_mesh)
        listenTo(_mesh)
      }
      
      val meshLoader = new FileChoiceAction("Load mesh...", main) {
        accelerator = "ctrl O"
        override def openFile(selected: File) = run {
          log("Loading mesh...")
          mesh = new Mesh(selected)
          log("Mesh with %d vertices and %d faces loaded." format
              (mesh.numberOfVertices, mesh.numberOfFaces))
        }
      }
      
      val morphLoader = new FileChoiceAction("Apply morph...", main) {
        override def openFile(selected: File) = run {
          log("Reading morph...")
          val morph = new Mesh(selected)
          log("Applying morph...")
          mesh = mesh.withMorphApplied(morph)
          log("Morph applied!")
        }
      }
      
      val meshSaver = new FileChoiceAction("Save mesh...", main) {
        accelerator = "ctrl S"
        openForWrite = true
        override def openFile(selected: File) = run {
          mesh.write(new FileWriter(selected), null)
    	  log("Wrote mesh to %s" format selected)
    	}
      }
      
      val screenShotSaver =
        new ScreenShotAction("Take Screen Shot...", main, sceneViewer) {
          accelerator = "ctrl I"
        }

      listenTo(meshLoader, screenShotSaver)
      reactions += {
        case MessageSent(src, text) => log(text)
        case FileChoiceAction.ChoiceCancelled(src) => log("Cancelled!")
        case FileChoiceAction.ChoiceError(src) => log("Error in file chooser.")
      }
  
      listenTo(sceneViewer, uvMapViewer)
      reactions += {
        case MessageSent(_, text) => log(text)
      }
      
      contents = main
      menuBar = new MenuBar {
        contents += new Menu("File") {
          contents ++ List(
            new MenuItem(meshLoader),
            new MenuItem(meshSaver),
            new Separator,
            new MenuItem(screenShotSaver),
            new Separator,
            new MenuItem(new Action("Exit") {
              def apply() { System.exit(0) }
              accelerator = "ctrl Q"
            })
          )
        }
        contents += new Menu("Mesh") {
          contents ++ List(
            new MenuItem(Action("Subdivide") { run { 
              log("Subdividing mesh...")
              mesh = mesh.subdivision
              log("Subdivision complete!")
            }}),
            new MenuItem(Action("Coarsen") { run { 
              log("Coarsening mesh...")
              mesh = mesh.coarsening
              log("Coarsening complete!")
            }}),
            new MenuItem(morphLoader)
          )
        }
      }
      pack
      visible = true
    }
  }
}
