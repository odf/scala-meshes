package org.gavrog.joss.meshes

class SparseVector(entries: (Int, Double)*) {
  import scala.collection.mutable.Map

  private var default: Double = 0.0
  private[this] val storage = Map[Int, Double]() 

  for (p <- entries if p._2 != 0.0) this(p._1) = p._2
  
  def this(a: SparseVector, f: Double => Double) =
	this(a.activeDomain.toList.map(i => (i, f(a(i)))) :_*)
  
  def this(a: SparseVector, b: SparseVector, f: (Double, Double) => Double) =
	this((a.activeDomain ++ b.activeDomain).toList
         .map(i => (i, f(a(i), b(i)))) :_*)
  
  def apply(key: Int) = storage.getOrElse(key, default)

  def items = storage.keys.map(i => (i, storage(i)))
  
  def activeDomain = storage.keySet
  
  def update(key: Int, value: Double) { storage(key) = value }

  def unary_- =	new SparseVector(this, x => -x)
  def *(f: Double) = new SparseVector(this, _ * f)
  def /(f: Double) = new SparseVector(this, _ / f)

  def +(that: SparseVector) = new SparseVector(this, that, _+_)
  def -(that: SparseVector) = new SparseVector(this, that, _-_)
}
