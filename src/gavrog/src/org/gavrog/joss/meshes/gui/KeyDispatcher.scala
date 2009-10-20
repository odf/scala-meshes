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

import java.awt.Point
import java.awt.event.InputEvent
import javax.swing.{JPopupMenu, KeyStroke}

import scala.swing.{Action, Alignment, GridPanel,
                    Frame, Label, Reactor, UIElement}
import scala.swing.event.{MouseClicked, MouseEntered, MousePressed, WindowClosed}
import scala.util.Sorting

trait KeyDispatcher extends Reactor with MessageSource {
  class Binding(description: String, code: => Unit) {
    def execute { code }
    override def toString = description
  }
  
  private var bindings = Map[KeyStroke, Binding]()
  private var boundKeys = List[KeyStroke]()
  private var sources = List[KeyPublisher]()
  
  def addKeySource(src: KeyPublisher) {
    sources = src :: sources
    listenTo(src.keyClicks)
  }
  
  def focusOnEnter(src: KeyPublisher) {
    listenTo(src.Mouse.moves)
    reactions += { case MouseEntered(`src`, _, _) => src.requestFocus }
  }
  
  def enableContextMenu(src: KeyPublisher) {
    listenTo(src.Mouse.clicks)
    reactions += {
      case MouseClicked(`src`, point, mods, _, _) => {
        val mask = InputEvent.META_DOWN_MASK | InputEvent.BUTTON3_DOWN_MASK
        if ((mods & mask) != 0) showKeyBindings(src, point)
      }
    }
  }
  
  def bind(ksText: String, description: String, code: => Unit) {
    KeyStroke.getKeyStroke(ksText) match {
      case null => throw new Error("Unknown key stroke '%s'" format ksText)
      case keyStroke => bind(keyStroke, description, code)
    }
  }
  
  def bind(keyStroke: KeyStroke, description: String, code: => Unit) {
    bindings += keyStroke -> new Binding(description, code)
    boundKeys = (boundKeys - keyStroke) ::: keyStroke :: Nil
  }
  
  def unbind(keyStroke: KeyStroke) {
    bindings  -= keyStroke
    boundKeys -= keyStroke
  }
  
  reactions += {
    case KeyPressed(src, modifiers, code, char) if (sources contains src) => {
      val ks = KeyStroke.getKeyStroke(code, modifiers)
      if (boundKeys contains ks) bindings(ks).execute
      else send("Not bound: '%s'" format ks)
    }
  }
  
  def bindingDescriptions = boundKeys.map(k => List(k, bindings(k).toString))
  
  def showKeyBindings(parent: UIElement, pos: Point) {
    JPopupMenu.setDefaultLightWeightPopupEnabled(false)
    new JPopupMenu("Key Bindings") {
      setLightWeightPopupEnabled(false)
      for (ks <- boundKeys) add(new Action(bindings(ks).toString) {
        def apply = bindings(ks).execute
        accelerator = Some(ks)
      }.peer)
    }.show(parent.peer, pos.x, pos.y)
  }
}
