package org.gavrog.joss.meshes

class SparseVector(entries: (Int, Double)*) {
  val items = Map() ++ entries.filter(_._2 != 0.0)

  def this(e: Iterable[(Int, Double)]) = this(e.toList :_*)
  
  def this(a: SparseVector, f: Double => Double) =
    this(a.support.map(i => (i, f(a(i)))))
  
  def this(a: SparseVector, b: SparseVector, f: (Double, Double) => Double) =
	this((a.support ++ b.support).map(i => (i, f(a(i), b(i)))))
  
  def apply(key: Int) = items.getOrElse(key, 0.0)

  def support = items.keySet
  
  def unary_- =	new SparseVector(this, x => -x)
  def *(f: Double) = new SparseVector(this, _ * f)
  def /(f: Double) = new SparseVector(this, _ / f)

  def +(that: SparseVector) = new SparseVector(this, that, _+_)
  def -(that: SparseVector) = new SparseVector(this, that, _-_)

  override def toString() = items.mkString("SparseVector(", ", ", ")")
  override def hashCode() = items.hashCode
  override def equals(other: Any) =	other.isInstanceOf[SparseVector] &&
    items.equals(other.asInstanceOf[SparseVector].items)
}
