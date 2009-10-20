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


package org.gavrog.joss.meshes.gui.actions

import java.io.File
import scala.swing.Component

import SwingSupport._

class ScreenShotAction(name: String, parent: Component,
                       viewer: JRealityViewerComponent)
extends FileChoiceAction(name, parent) with MessageSource
{
  openForWrite = true
  
  override def openFile(selected: File) = run {
    send("Taking screenshot ...")
    val d = viewer.size
    viewer.screenshot((d.width, d.height), 4, selected)
    send("Wrote image to %s." format selected.getName)
  }
}
