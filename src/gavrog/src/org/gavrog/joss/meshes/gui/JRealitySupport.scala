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

import de.jreality.math.MatrixBuilder
import de.jreality.scene.{Appearance, Transformation}

object JRealitySupport {
  implicit def asTransformation(mb: MatrixBuilder) = new Transformation {
    mb.assignTo(this)
  }
  
  class RichAppearance extends Appearance {
    private def update(attr: Iterable[(String, Any)]) {
      for ((k, v) <- attr) setAttribute(k, v)
    }
    
    def this(attr: (String, Any)*) {
      this()
      update(attr)
    }
    
    def this(attr: Map[String, Any]) {
      this()
      update(attr)
    }
  }
}
