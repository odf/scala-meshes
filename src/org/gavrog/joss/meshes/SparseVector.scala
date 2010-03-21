package org.gavrog.joss.meshes

class SparseVector(val size: Int, entries: (Int, Double)*) {
  import scala.collection.mutable.Map

  private var default: Double = 0.0
  private[this] val storage = Map[Int, Double]() 

  for (p <- entries) this(p._1) = p._2
  
  def this(old: SparseVector, f: Double => Double) =
	this(old.size, old.items.toList.map(p => (p._1, f(p._2))) :_*)
  
  def this(a: SparseVector, b: SparseVector, f: (Double, Double) => Double) =
	this(a.size max b.size, (a.activeDomain ++ b.activeDomain).toList
      .map(i => (i, f(a(i), b(i)))).filter(_._2 != 0.0) :_*)
  
  def apply(key: Int) = 
    if(key >= 0 && key < size)
      storage.getOrElse(key, default)
    else 
      throw new IllegalArgumentException("Index "  + key + " out of bounds")

  def items = storage.keys.map(i => (i, storage(i)))
  
  def activeDomain = storage.keySet
  
  def update(key: Int, value: Double) { storage(key) = value }

  def unary_- =	new SparseVector(this, x => -x)
  def *(f: Double) = new SparseVector(this, _ * f)
  def /(f: Double) = new SparseVector(this, _ / f)

  def +(that: SparseVector) = new SparseVector(this, that, _+_)
  def -(that: SparseVector) = new SparseVector(this, that, _-_)
}
