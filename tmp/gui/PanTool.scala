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


package org.gavrog.joss.meshes.gui;

import de.jreality.math.{FactoredMatrix, Matrix, MatrixBuilder}
import de.jreality.scene.Transformation
import de.jreality.scene.tool.{AbstractTool, InputSlot, ToolContext}

import JRealitySupport._

class PanTool extends AbstractTool {
  object Axis extends Enumeration {
    val X, Y, None = Value
  }

  val activationSlot = InputSlot.getDevice("DragActivation")
  val restrictionSlot = InputSlot.getDevice("Secondary")
  val evolutionSlot = InputSlot.getDevice("PointerEvolution")
  addCurrentSlot(activationSlot)
  addCurrentSlot(restrictionSlot)
  addCurrentSlot(evolutionSlot)

  var restricted = false
  var lastAxis = Axis.None
  
  override def perform(tc: ToolContext) {
    if (tc.getAxisState(activationSlot).isReleased) {
      restricted = false
      return
    }
    if (restricted == false && tc.getAxisState(restrictionSlot).isPressed) {
      restricted = true
      lastAxis = Axis.None
    }
    val evolution =
      new FactoredMatrix(tc.getTransformationMatrix(evolutionSlot))
    val t = evolution.getTranslation
    if (restricted) {
      if (lastAxis == Axis.None) lastAxis = (t(0).abs, t(1).abs) match {
        case (x, y) if x > y => Axis.X
        case (x, y) if x < y => Axis.Y
        case _               => return
      }
      if (lastAxis == Axis.X) {
        t(1) = 0.0
        t(2) = 0.0
      } else {
        t(0) = 0.0
        t(2) = 0.0
      }
    }
    var path = tc.getRootToToolComponent
    MatrixBuilder.euclidean.translate(t)
      .times(path.getMatrix(null)).assignTo(path.getLastComponent)
  }
}
