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


package org.gavrog.joss.meshes

import Sums._

object Vectors {
  case class Vec2(x: Double, y: Double) {
    def unary_- = Vec2(-x, -y)
    def +(that: Vec2) = Vec2(this.x + that.x, this.y + that.y)
    def -(that: Vec2) = Vec2(this.x - that.x, this.y - that.y)
    def *(f: Double) = Vec2(x * f, y * f)
    def /(f: Double) = Vec2(x / f, y / f)
    
    def *(that: Vec2) = this.x * that.x + this.y * that.y
    def norm = Math.sqrt(this * this)
    def || = norm
    def unit = this / this.||

    def toArray = Array(x, y)
    def toList = List(x, y)
  }
  object zero2 extends Vec2(0, 0)
  implicit object Vec2Monoid extends Monoid[Vec2] {
    def add(x: Vec2, y: Vec2) = x + y
    def unit = Vec2(0, 0)
  }
  implicit def asArray(v: Vec2) = v.toArray
  
  case class Vec3(x: Double, y: Double, z: Double) {
    def unary_- = Vec3(-x, -y, -z)
    def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)
    def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)
    def *(f: Double) = Vec3(x * f, y * f, z * f)
    def /(f: Double) = Vec3(x / f, y / f, z / f)
    
    def *(that: Vec3) = this.x * that.x + this.y * that.y + this.z * that.z
    def x(that: Vec3) : Vec3 = Vec3(this.y * that.z - this.z * that.y,
                                    this.z * that.x - this.x * that.z,
                                    this.x * that.y - this.y * that.x)
    def norm = Math.sqrt(this * this)
    def || = norm
    def unit = this / this.||

    def toArray = Array(x, y, z)
    def toList = List(x, y, z)
  }
  object zero3 extends Vec3(0, 0, 0)
  implicit object Vec3Monoid extends Monoid[Vec3] {
    def add(x: Vec3, y: Vec3) = x + y
    def unit = Vec3(0, 0, 0)
  }
  implicit def asArray(v: Vec3) = v.toArray
  
  case class Scalar(x: Double) {
    def *(that: Vec3) = that * x
  }
  implicit def dbl2scalar(x: Double) = Scalar(x)
  implicit def int2scalar(x: Int) = Scalar(x)
}
