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

import java.awt.event.KeyListener
import javax.swing.JComponent

import scala.swing.{Component, Publisher}
import scala.swing.event.InputEvent

class KeyEvent(val source: Component,
               val modifiers: Int,
               keyCode: Int, keyChar: Char)(val when: Long)
extends InputEvent

case class KeyPressed(override val source: Component,
                      override val modifiers: Int, 
                      keyCode: Int, keyChar: Char)(when: Long)
extends KeyEvent(source, modifiers, keyCode, keyChar)(when)

case class KeyReleased(override val source: Component,
                       override val modifiers: Int, 
                       keyCode: Int, keyChar: Char)(when: Long)
extends KeyEvent(source, modifiers, keyCode, keyChar)(when)

case class KeyTyped(override val source: Component,
                    override val modifiers: Int, 
                    keyCode: Int, keyChar: Char)(when: Long)
extends KeyEvent(source, modifiers, keyCode, keyChar)(when)

trait KeyPublisher extends Component {
  object keyClicks extends Publisher {
    peer.addKeyListener(new KeyListener {
      def wrapped[C<:Component](e: java.awt.event.ComponentEvent) =
        e.getSource.asInstanceOf[JComponent]
          .getClientProperty("scala.swingWrapper").asInstanceOf[C]
      
      def keyPressed (e: java.awt.event.KeyEvent) = publish(KeyPressed(
        wrapped(e), e.getModifiers, e.getKeyCode, e.getKeyChar)(e.getWhen))
      
      def keyReleased(e: java.awt.event.KeyEvent) = publish(KeyReleased(
        wrapped(e), e.getModifiers, e.getKeyCode, e.getKeyChar)(e.getWhen))
      
      def keyTyped   (e: java.awt.event.KeyEvent) = publish(KeyTyped(
        wrapped(e), e.getModifiers, e.getKeyCode, e.getKeyChar)(e.getWhen))
    })
  }
}
