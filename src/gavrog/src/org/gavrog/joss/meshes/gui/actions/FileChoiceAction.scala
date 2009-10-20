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

import scala.swing.{Action, Component, FileChooser, Publisher}
import scala.swing.event.Event

object FileChoiceAction {
  abstract class FileChoiceEvent(src: FileChoiceAction) extends Event
  case class FileChosen(src: FileChoiceAction,
                        file: File) extends FileChoiceEvent(src)
  case class ChoiceCancelled(src: FileChoiceAction) extends FileChoiceEvent(src)
  case class ChoiceError(src: FileChoiceAction) extends FileChoiceEvent(src)
}

class FileChoiceAction(name: String, parent: Component)
extends Action(name) with Publisher
{
  private val chooser = new FileChooser { title = name }
  var openForWrite = false
  
  def apply {
	import FileChooser.Result._
	val method = if (openForWrite) chooser.showSaveDialog _
				 else chooser.showOpenDialog _
  	method(parent) match {
  	  case Approve => openFile(chooser.selectedFile)
  	  case Cancel  => publish(FileChoiceAction.ChoiceCancelled(this))
  	  case Error   => publish(FileChoiceAction.ChoiceError(this))
	}
  }
  
  def openFile(selected: File) {
    publish(FileChoiceAction.FileChosen(this, selected))
  }
}
