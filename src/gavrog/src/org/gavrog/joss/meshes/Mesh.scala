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

import java.io._

import scala.Math
import scala.collection.mutable._
import scala.io.Source
import Sums._
import Vectors._

object Mesh {
  trait Chamber {
    def s0: Chamber
    def s1: Chamber
    def s2: Chamber
    
    def vertex: Vertex
    def tVertex: TextureVertex
    def normal: Normal
    def cell: Cell

    def s0_=(ch: Chamber)
    def s1_=(ch: Chamber)
    def s2_=(ch: Chamber)

    def tVertex_=(t: TextureVertex)
    def normal_=(n: Normal)

    def mesh = vertex.mesh
    
    def vertexNr = if (vertex != null) vertex.nr else 0
    def tVertexNr = if (tVertex != null) tVertex.nr else 0
    def normalNr = if (normal != null) normal.nr else 0
    
    def face = if (cell.isInstanceOf[Face]) cell.asInstanceOf[Face] else null
    
    def nextAtFace   = s0.s1
    def prevAtFace   = s1.s0
    def nextAtVertex = s1.s2
    def prevAtVertex = s2.s1
    def opposite     = s0.s2
    
    def start        = vertex
    def end          = s0.vertex
    
    def onTextureBorder = tVertex != s2.tVertex || s0.tVertex != s0.s2.tVertex
    
    def orbit(next: Chamber => Chamber) = {
      def from(c: Chamber): Stream[Chamber] =
        if (c == this) Stream.empty else Stream.cons(c, from(next(c)))
      Stream.cons(this, from(next(this)))
    }
    
    def cellChambers = orbit(_.nextAtVertex)
    def vertexChambers = orbit(_.nextAtFace)
    
    //override def toString = "Chamber(%s -> %s)" format (start, end)
  }
  
  trait Vertex {
    def mesh   : Mesh
    def nr     : Int
    def chamber: Chamber
    def pos    : Vec3

    def pos_=(p: Vec3)

    def cellChambers = chamber.cellChambers
    def degree       = cellChambers.size
    def faces        = cellChambers.map(_.cell)
    def neighbors    = cellChambers.map(_.s0.vertex)
    def x = pos.x
    def y = pos.y
    def z = pos.z

    def pos_=(p: (Double, Double, Double)) { pos = Vec3(p._1, p._2, p._3) }

    override def toString = "Vertex(%d)" format (nr)
  }
  
  trait TextureVertex {
    def mesh   : Mesh
    def nr     : Int
    def chamber: Chamber
    def pos    : Vec2

    def pos_=(p: Vec2)
    def chamber_=(c: Chamber)

    def cellChambers = chamber.cellChambers
    def faces        = cellChambers.filter(_.tVertex == this).map(_.cell)
    def onBorder     = cellChambers.exists(_.onTextureBorder)

    def degree = cellChambers.sum(c =>
      if (c.tVertex == this || c.s2.tVertex == this)
        if (c.tVertex == c.s2.tVertex && c.s0.tVertex != c.s2.s0.tVertex) 2
        else 1
      else 0
    )

    def x = pos.x
    def y = pos.y
    def z = 0.0
    
    def pos_=(p: (Double, Double)) { pos = Vec2(p._1, p._2) }
    
    override def toString = "TextureVertex(%d)" format (nr)
  }

  trait Normal {
    def mesh   : Mesh
    def nr     : Int
    def chamber: Chamber
    def value  : Vec3

    def value_=(p: Vec3)
    def chamber_=(c: Chamber)

    def x = value.x
    def y = value.y
    def z = value.z

    def value_=(p: (Double, Double, Double)) { value = Vec3(p._1, p._2, p._3) }

    override def toString = "Normal(%d)" format (nr)
  }

  trait Cell {
    def mesh   : Mesh
    def chamber: Chamber
    
    def vertexChambers = chamber.vertexChambers
    def vertices = vertexChambers.map(_.vertex)
    def degree   = vertexChambers.size
    
    def formatVertices(v0: Int, vt0: Int, vn0: Int) =
      vertexChambers.map(ch =>
        "%d/%s/%s" format (ch.start.nr + v0,
                           if (ch.tVertex != null) ch.tVertex.nr + vt0 else "",
                           if (ch.normal  != null) ch.normal.nr  + vn0 else "")
      ).mkString(" ")

    def formatVertices: String = formatVertices(0, 0, 0)
  }
  
  trait Face extends Cell {
    def obj           : Object
    def group         : Group
    def material      : Material
    def smoothingGroup: Int
    
    def obj_=           (o: Object)
    def group_=         (g: Group)
    def material_=      (m: Material)
    def smoothingGroup_=(s: Int)
    
    def textureVertices = vertexChambers.map(_.tVertex)
    def normals = vertexChambers.map(_.normal)

    override def toString = "Face(%s; group = '%s', material = '%s')" format
      (formatVertices, group.name, material.name)
  }
  trait Hole extends Cell {
    override def toString = "Hole(%s)" format formatVertices
  }

  class Edge(v: Vertex, w: Vertex) {
    val (from, to) = if (v.nr <= w.nr) (v, w) else (w, v)
    
    override def equals (other: Any) = other.isInstanceOf[Edge] && {
      val e = other.asInstanceOf[Edge]
      e.from.nr == from.nr && e.to.nr == to.nr
    }
    
    override def hashCode = from.nr * 37 + to.nr
  }
  
  case class Object(name: String) {
    override def toString = "Object(" + name + ")"
  }
  
  case class Group(name: String) {
    override def toString = "Group(" + name + ")"
  }
  
  case class Material(name: String) {
    override def toString = "Material(" + name + ")"
  }

  case class Component(mesh: Mesh, chambers: Set[Chamber]) {
    lazy val vertices = new HashSet[Vertex] ++ chambers.map (_.vertex)
    lazy val faces = new HashSet[Face] ++ chambers.map(_.face).filter(null !=)
    def coarseningClassifications = faces.elements.next.vertices
      .map(mesh.classifyForCoarsening(_)).filter(null !=)
  }
  
  case class Chart(mesh: Mesh, chambers: Set[Chamber]) {
    lazy val vertices = new HashSet[TextureVertex] ++ chambers.map (_.tVertex)
    lazy val faces = new HashSet[Face] ++ chambers.map (_.face)
  }
  
  case class VertexClassification(wasVertex:     Set[Vertex],
                                  wasEdgeCenter: Set[Vertex],
                                  wasFaceCenter: Set[Vertex])
  {
    def cost(v: Vertex) = {
      var n = 0
      if (wasFaceCenter(v)) {
        for (c <- v.cellChambers) {
          val d = c.s2
          if (c.tVertex != d.tVertex) n += 3
          if (c.face != null) {
        	if (c.face.material != d.face.material) n += 1
        	if (c.face.group != d.face.group) n += 1
          }
        }
      }
      n
    }

    def cost: Int = wasFaceCenter.sum(cost(_))
    def isStrict = cost == 0
  }

  def matchTopologies(ch1: Chamber, ch2: Chamber,
                      uvs: Boolean): Map[Chamber, Chamber] = {
    val seen1 = new HashSet[Chamber]
    val seen2 = new HashSet[Chamber]
    val queue = new Queue[(Chamber, Chamber)]
    val map   = new HashMap[Chamber, Chamber]
    seen1 += ch1
    seen2 += ch2
    queue += (ch1, ch2)
    map(ch1) = ch2
    
    def neighbors(c: Chamber) =
      List(c.s0, c.s1, if (uvs && c.onTextureBorder) null else c.s2)
    
    while (queue.length > 0) {
      val (d1, d2) = queue.dequeue
      for ((e1, e2) <- neighbors(d1).zip(neighbors(d2))) {
        if ((e1 == null) != (e2 == null)) return null
        if (e1 != null) {
	        if (seen1(e1) != seen2(e2)) return null
	        if (e1.cell.getClass != e2.cell.getClass) return null
	        if (seen1(e1)) {
	          if (map(e1) != e2) return null
	        } else {
	          queue += (e1, e2)
	          seen1 += e1
	          seen2 += e2
	          map(e1) = e2
	        }
         }
      }
    }
    map
  }
  
  def distance[T <: { def x: Double; def y: Double; def z: Double }](
    map: Map[Chamber, Chamber], v: Chamber => T) =
  {
    val verts = new HashSet[(T, T)]
    for ((c, d) <- map) verts += ((v(c), v(d)))
    
    var dist: Double = 0
    for ((v, w) <- verts) {
      val (dx, dy, dz) = (w.x - v.x, w.y - v.y, w.z - v.z)
      dist += dx * dx + dy * dy + dz * dz
    }
    dist
  }
  
  def closest[T <: { def x: Double; def y: Double; def z: Double }](
    maps: Seq[Map[Chamber, Chamber]], v: Chamber => T) =
  {
    var best: Map[Chamber, Chamber] = null
    var dist = Double.MaxValue
    
    for (map <- maps) {
      val d = distance(map, v)
      if (d < dist) {
        dist = d
        best = map
      }
    }
    best
  }

  //TODO avoid code duplication in the following
  def allMatches(c1: Component, c2: Component): Seq[Map[Chamber, Chamber]] = {
    val result = new ArrayBuffer[Map[Chamber, Chamber]]
    if (c1.chambers.size != c2.chambers.size) return result
    
    val degree = new HashMap[Vertex, Int]
    val count = new HashMap[Int, Int]
    for (v <- c1.vertices) {
      degree(v) = v.degree
      count(degree(v)) = count.getOrElse(degree(v), 0) + 1
    }
    for (v <- c2.vertices) degree(v) = v.degree

    val bestD = count.keys.toList.sort((a, b) => count(a) < count(b))(0)

    val ch1 = c1.chambers.find(ch => degree(ch.vertex) == bestD).get
    for (ch2 <- c2.chambers if degree(ch2.vertex) == bestD)
      matchTopologies(ch1, ch2, false) match {
        case null => ()
        case map  => result += map
      }
    
    result
  }
  
  def allMatches(c1: Chart, c2: Chart): Seq[Map[Chamber, Chamber]] = {
    val result = new ArrayBuffer[Map[Chamber, Chamber]]
    if (c1.chambers.size != c2.chambers.size) return result
    
    val degree = new HashMap[TextureVertex, Int]
    val count = new HashMap[Int, Int] { this(0) = c1.chambers.size }
    for (t <- c1.vertices) {
      degree(t) = if (t.onBorder) t.degree else 0
      count(degree(t)) = count.getOrElse(degree(t), 0) + 1
    }
    for (t <- c2.vertices) degree(t) = if (t.onBorder) t.degree else 0
    
    val bestD = count.keys.toList.sort((a, b) => count(a) < count(b))(0)

    val ch1 = c1.chambers.find(ch => degree(ch.tVertex) == bestD).get
    for (ch2 <- c2.chambers if degree(ch2.tVertex) == bestD)
      matchTopologies(ch1, ch2, false) match {
        case null => ()
        case map  => result += map
      }
    
    result
  }
}

class Mesh extends MessageSource {
  import Mesh._

  private val _vertices = new ArrayBuffer[Vertex]
  private val _normals  = new ArrayBuffer[Normal]
  private val _texverts = new ArrayBuffer[TextureVertex]
  private val _faces    = new ArrayBuffer[Face]
  private val _holes    = new ArrayBuffer[Hole]
  private val _chambers = new ArrayBuffer[Chamber]
  private val _objects  = new LinkedHashMap[String, Object]
  private val _groups   = new LinkedHashMap[String, Group]
  private val _mats     = new LinkedHashMap[String, Material]
  private val _edges    = new HashMap[Edge, Chamber]
  
  private val _s0 = new HashMap[Chamber, Chamber]
  private val _s1 = new HashMap[Chamber, Chamber]
  private val _s2 = new HashMap[Chamber, Chamber]
  
  private val _chamber_at_vertex  = new HashMap[Vertex, Chamber]
  private val _chamber_at_tvertex = new HashMap[TextureVertex, Chamber]
  private val _chamber_at_cell    = new HashMap[Cell, Chamber]
  
  private val _vertex_pos  = new HashMap[Vertex, Vec3]
  private val _texture_pos = new HashMap[TextureVertex, Vec2]
  private val _normal_val  = new HashMap[Normal, Vec3]
  
  private val _object_for_face          = new HashMap[Face, Object]
  private val _group_for_face           = new HashMap[Face, Group]
  private val _material_for_face        = new HashMap[Face, Material]
  private val _smoothing_group_for_face = new HashMap[Face, Int]

  object vertex_position {
    def apply(v: Vertex) = _vertex_pos(v)
    def update(v: Vertex, p: Vec3) { _vertex_pos(v) = p }
  }
  
  object texture_position {
    def apply(t: TextureVertex) = _texture_pos(t)
    def update(t: TextureVertex, p: Vec2) { _texture_pos(t) = p }
  }
  
  object normal_value {
    def apply(n: Normal) = _normal_val(n)
    def update(n: Normal, p: Vec3) { _normal_val(n) = p }
  }
  
  val mtllib = new HashMap[String, String]

  def this(source: Source) {
    this()
    
    val faces = new ArrayBuffer[(Seq[Int], Seq[Int], Seq[Int],
                                 Object, Group, Material, Int)]
    var obj     : Object   = null
    var group   : Group    = null
    var material: Material = null
    var smoothingGroup = 0
    val mtllib = new HashMap[String, String]
    
    for(raw <- source.getLines; line = raw.trim
        if line.length > 0 && !line.startsWith("#")) {
      val fields: Seq[String] = line.split("\\s+")
      val cmd = fields(0)
      val pars = fields.slice(1, fields.length)

      cmd match {
      case "mtllib" => {
        try {
          var curmtl: String = null
          for(raw <- Source.fromFile(pars(0)).getLines;
              line = raw.trim
              if line.length > 0 && !line.startsWith("#")) {
            val fields: Seq[String] = line.split("\\s+")
            if (fields(0) == "newmtl") {
              curmtl = fields(1)
              mtllib(curmtl) = ""
            }
            else mtllib(curmtl) += line + "\n"
          }
        } catch {
          case e: FileNotFoundException => System.err.println(
            "WARNING: material file %s not found." format pars(0))
        }
      }
      case "v"  => {
        addVertex(pars(0).toDouble, pars(1).toDouble, pars(2).toDouble)
      }
      case "vn" => {
        addNormal(pars(0).toDouble, pars(1).toDouble, pars(2).toDouble)
      }
      case "vt" => {
        addTextureVertex(pars(0).toDouble, pars(1).toDouble)
      }
      case "o" => {
        if (pars.size > 0) obj = this obj pars(0)
      }
      case "g"  => {
        if (pars.size > 0) group = this group pars(0)
      }
      case "usemtl"  => {
        if (pars.size > 0) material = this material pars(0)
      }
      case "s" => {
        if (pars.size > 0) smoothingGroup = pars(0).toInt
      }
      case "f"  => {
        val n  = pars.length
        val fc = new ArrayBuffer[Int]
        val ft = new ArrayBuffer[Int]
        val fn = new ArrayBuffer[Int]
        pars.foreach { s =>
          val parts = (s + "/0/0").split("/").map(
            (s: String) => if (s.length == 0) 0 else s.toInt)
          fc += parts(0) + (if (parts(0) < 0) numberOfVertices + 1 else 0)
          ft += parts(1) + (if (parts(1) < 0) numberOfTextureVertices + 1 else 0)
          fn += parts(2) + (if (parts(2) < 0) numberOfNormals + 1 else 0)
        }
        if (obj == null) obj = this.obj("_default")
        if (group == null) group = this.group("_default")
        if (material == null) material = this.material("_default")
        faces += (fc, ft, fn, obj, group, material, smoothingGroup)
      }
      case _ => println("?? " + cmd + "(" + pars.mkString(", ") + ")")
      }
    }
    addFaces(faces)
    fixHoles
  }

  def this(in: java.io.InputStream) = this(Source.fromInputStream(in))
  def this(file: File) = this(Source.fromFile(file))
  
  def write(target: OutputStream, basename: String) {
    write(new OutputStreamWriter(target), basename)
  }

  def write(target: Writer, basename: String) {
    val writer = new BufferedWriter(target)
    
    if (basename != null) {
      val mtl = new BufferedWriter(new FileWriter("%s.mtl" format basename))
      for ((name, definition) <- mtllib)
        mtl.write("newmtl %s\n%s\n" format (name, definition))
      mtl.flush
      mtl.close
      writer.write("mtllib %s.mtl\n" format basename)
    }
    
    for (v <- vertices)
      writer.write("v %.8f %.8f %.8f\n" format (v.x, v.y, v.z))
    for (v <- normals)
      writer.write("vn %.8f %.8f %.8f\n" format (v.x, v.y, v.z))
    for (v <- textureVertices)
      writer.write("vt %.8f %.8f\n" format (v.x, v.y))
    
    val parts = new LinkedHashMap[(Object, Group, Material, Int), Buffer[Face]]
    var useSmoothing = false
    for (f <- faces) {
      parts.getOrElseUpdate((f.obj, f.group, f.material, f.smoothingGroup),
                            new ArrayBuffer[Face]) += f
      useSmoothing ||= (f.smoothingGroup != 0)
    }
    for (((obj, group, material, smoothingGroup), faces) <- parts) {
      writer.write("o %s\n" format obj.name)
      writer.write("g %s\n" format group.name)
      writer.write("usemtl %s\n" format material.name)
      if (useSmoothing) writer.write("s %d\n" format smoothingGroup)
      for (f <- faces) writer.write("f %s\n" format f.formatVertices)
    }
    writer.flush()
  }
  
  override def clone = {
    val w = new StringWriter
    write(w, null)
    new Mesh(Source.fromString(w.toString))
  }

  private def addChamber(v: Vertex, f: Cell) = new Chamber {
    private var _vertex = v
    private var _cell   = f

    private var _tVertex: TextureVertex = null
    private var _normal : Normal        = null
    
    def vertex = _vertex
    
    def tVertex = _tVertex
    def tVertex_=(t: TextureVertex) {
      _tVertex = t
      if (t != null && t.chamber == null) t.chamber = this
    }
    
    def normal = _normal
    def normal_=(n: Normal) {
      _normal = n
      if (n != null && n.chamber == null) n.chamber = this
    }
    
    def cell = _cell
    
    def s0 = _s0.getOrElse(this, null)
    def s1 = _s1.getOrElse(this, null)
    def s2 = _s2.getOrElse(this, null)
    
    def setOperator(s: Map[Chamber, Chamber], ch: Chamber) {
      if (s.contains(this)) s.removeKey(s(this))
      if (ch == null) s.removeKey(this)
      else {
    	if (s.contains(ch)) s.removeKey(s(ch))
    	s(this) = ch
    	s(ch) = this
      }
    }
    
    def s0_=(ch: Chamber) { setOperator(_s0, ch) }
    def s1_=(ch: Chamber) { setOperator(_s1, ch) }
    def s2_=(ch: Chamber) { setOperator(_s2, ch) }
    
    _chambers += this
    if (_chamber_at_vertex.get(v) == None) _chamber_at_vertex(v) = this
    if (_chamber_at_cell.get(f)   == None) _chamber_at_cell(f)   = this
  }
  
  def numberOfChambers = _chambers.size
  def chambers         = _chambers.elements
  def hardChambers     = {
    val smoothing =
      !chambers.forall(c => c.face == null || c.face.smoothingGroup == 0)
    chambers.filter(c => {
      val (f, g) = (c.face, c.s2.face)
      if (f == null || g == null)
        true
      else if (smoothing)
        f.smoothingGroup == 0 || g.smoothingGroup == 0 ||
        f.smoothingGroup != g.smoothingGroup
      else
        false
    })
  }
  
  def addVertex(p: Vec3): Vertex = addVertex(p.x, p.y, p.z)
  
  def addVertex(_x: Double, _y: Double, _z: Double) = new Vertex {
    val mesh = Mesh.this
    val nr = numberOfVertices + 1
    
    def chamber = _chamber_at_vertex(this)
    
    def pos = vertex_position(this)
    def pos_=(p: Vec3) { vertex_position(this) = p }
    
    _vertices += this
    pos = (_x, _y, _z)
  }
  
  def numberOfVertices = _vertices.size
  def vertices         = _vertices.elements
  def vertex(n: Int)  =
    if (n > 0 && n <= numberOfVertices) _vertices(n - 1) else null
  
  def clearTextureVertices = _texverts.clear

  def addTextureVertex(p: Vec2): TextureVertex = addTextureVertex(p.x, p.y)
  
  def addTextureVertex(_x: Double, _y: Double) = new TextureVertex {
    val mesh = Mesh.this
    val nr = numberOfTextureVertices + 1
    
    private var _ch: Chamber = null    
    def chamber = _ch
    def chamber_=(c: Chamber) {
      _ch = c
      if (c != null && c.tVertex != this) c.tVertex = this
    }
    
    def pos = texture_position(this)
    def pos_=(p: Vec2) { texture_position(this) = p }

    _texverts += this
    pos = (_x, _y)
  }

  def numberOfTextureVertices = _texverts.size
  def textureVertices         = _texverts.elements
  def textureVertex(n: Int)  =
    if (n > 0 && n <= numberOfTextureVertices) _texverts(n - 1) else null
  
  def clearNormals = _normals.clear
  
  def addNormal(p: Vec3): Normal = addNormal(p.x, p.y, p.z)
  
  def addNormal(_x: Double, _y: Double, _z: Double) = new Normal {
    val mesh = Mesh.this
    val nr = numberOfNormals + 1
    
    private var _ch: Chamber = null    
    def chamber = _ch
    def chamber_=(c: Chamber) {
      _ch = c
      if (c != null && c.normal != this) c.normal = this
    }
    
    def value = normal_value(this)
    def value_=(p: Vec3) { normal_value(this) = p }

    _normals += this
    value = (_x, _y, _z)
  }
  
  def numberOfNormals  = _normals.size
  def normals          = _normals.elements
  def normal(n: Int)  =
    if (n > 0 && n <= numberOfNormals) _normals(n - 1) else null

  def numberOfEdges    = _edges.size
  def edges            = _edges.keys
  
  def numberOfFaces    = _faces.size
  def faces            = _faces.elements

  def numberOfHoles    = _holes.size
  def holes            = _holes.elements

  def obj(name: String) = _objects.get(name) match {
    case Some(o) => o
    case None    => val o = new Object(name); _objects.put(name, o); o
  }
  def numberOfObjects = _objects.size
  def objects         = _objects.values
  def clearObjects    = _objects.clear
  
  def group(name: String) = _groups.get(name) match {
    case Some(g) => g
    case None    => val g = new Group(name); _groups.put(name, g); g
  }
  def numberOfGroups = _groups.size
  def groups         = _groups.values
  def clearGroups    = _groups.clear
  
  def material(name: String) = _mats.get(name) match {
    case Some(m) => m
    case None    => val m = new Material(name); _mats.put(name, m); m
  }
  def numberOfMaterials = _mats.size
  def materials         = _mats.values
  def clearMaterials    = _mats.clear

  def addFace(verts   : Seq[Int], tverts  : Seq[Int], normvecs: Seq[Int]) = {
    val f = new Face {
      val mesh = Mesh.this
      def chamber = _chamber_at_cell(this)
      
      def obj            = _object_for_face(this)
      def group          = _group_for_face(this)
      def material       = _material_for_face(this)
      def smoothingGroup = _smoothing_group_for_face(this)
      
      def obj_=(o: Object)         { _object_for_face(this) = o }
      def group_=(g: Group)        { _group_for_face(this) = g }
      def material_=(m: Material)  { _material_for_face(this) = m }
      def smoothingGroup_=(s: Int) { _smoothing_group_for_face(this) = s }
    }
    
    val n = verts.length
    val chambers = new ArrayBuffer[Chamber]
    for (i <- 0 until n; j <- List(i, (i + 1) % n)) {
      val c = addChamber(vertex(verts(j)), f)
      c.tVertex = textureVertex(tverts(j))
      c.normal  = normal(normvecs(j))
      chambers  += c
    }
    
    for (i <- 0 until n) {
      val c = chambers(2*i)
      c.s0  = chambers(2*i + 1)
      c.s1  = chambers((i + n - 1) % n * 2 + 1)
      val e = new Edge(c.start, c.end)
      var d = _edges.getOrElse(e, null)
      if (d != null) {
        if (d.s2 != null) error("More than two faces at " + c)
        if (c.start != d.start) d = d.s0
        c.s2    = d
        c.s0.s2 = d.s0
      }
      _edges(e) = c
    }
    
    _faces += f
    f
  }
  
  def addFaces(faces: Seq[(Seq[Int], Seq[Int], Seq[Int],
                           Object, Group, Material, Int)]) =
    for ((fc, ft, fn, o, g, mtl, s) <- faces) {
      val f = addFace(fc, ft, fn)
      f.obj            = o
      f.group          = g
      f.material       = mtl
      f.smoothingGroup = s
    }
  
  def fixHoles {
    val seen = new HashSet[Chamber]
    for (c <- chambers if c.s2 == null && !seen(c)) {
      val boundary = new ArrayBuffer[Chamber]
      var d = c
      do {
        boundary += d
        boundary += d.s0
        d = d.s0.s1
        while (d.s2 != null) d = d.s2.s1
      } while (d != c)
      seen ++ boundary
      val n = boundary.length
      
      val f = new Hole {
        val mesh = Mesh.this
        def chamber = _chamber_at_cell(this)
      }

      val hole = boundary.map(d => addChamber(d.vertex, f)).toSeq
      for (i <- 0 until n) {
        val d = hole(i)
        d.s2 = boundary(i)
        if (i % 2 == 0) d.s0 = hole(i + 1) else d.s1 = hole((i + 1) % n)
      }
      for (d <- hole) d.tVertex = null
      
      _holes += f
    }
  }
  
  def computeNormals = {
    clearNormals
    val normal4face = new LazyMap((f: Cell) =>
      f.vertexChambers.sum(c => c.vertex.pos x c.nextAtFace.vertex.pos).unit)
    for (v <- vertices) {
      val n = addNormal(v.cellChambers.sum(c =>
        if (c.cell.isInstanceOf[Face]) normal4face(c.cell) else zero3).unit)
      for (c <- v.cellChambers; d <- List(c, c.s1)) d.normal = n
    }
  }
  
  def components = {
    val result = new ArrayBuffer[Component]
    val seen   = new HashSet[Chamber]
    
    for (c <- chambers if !seen(c)) {
      val chambers = new HashSet[Chamber]
      val queue    = new Queue[Chamber]
      queue += c
      seen  += c
      
      while (queue.length > 0) {
        val d = queue.dequeue
        chambers += d
        val f = d.cell
        for (e <- List(d.s0, d.s1, d.s2) if !seen(e)) {
          queue += e
          seen  += e
        }
      }
      
      result += Component(this, chambers)
    }
    
    result
  }
  
  def charts = {
    val result = new ArrayBuffer[Chart]
    val seen   = new HashSet[Chamber]
    
    for (c <- chambers if !seen(c) && c.tVertex != null) {
      val chambers  = new HashSet[Chamber]
      val queue  = new Queue[Chamber]
      queue += c
      seen  += c
      
      while (queue.length > 0) {
        val d = queue.dequeue
        val t = d.tVertex
        if (t != null) {
          chambers += d
          val f = d.cell
          for (e <- List(d.s0, d.s1) if !seen(e)) {
            queue += e
            seen  += e
          }
          val e = d.s2
          if (e != null && !seen(e) && e.tVertex == t) {
            queue += e
            seen  += e
          }
        }
      }
      
      result += Chart(this, chambers)
    }
    
    result
  }
  
  def splitByGroup = {
    val parts = new HashMap[String, Buffer[Face]]
    for (g <- _groups.values) parts(g.name) = new ArrayBuffer[Face]
    for (f <- _faces) parts(f.group.name) += f
    split(parts)
  }
  
  def splitByMaterial = {
    val parts = new HashMap[String, Buffer[Face]]
    for (m <- _mats.values) parts(m.name) = new ArrayBuffer[Face]
    for (f <- _faces) parts(f.material.name) += f
    split(parts)
  }
  
  def split(parts: Iterable[(String, Seq[Face])]) =
    for ((part_name, faces) <- parts if faces.size > 0) yield {
      val m = new Mesh()
      val vMap = Map(0 -> 0) ++ {
    	val vSet = Set() ++ faces.flatMap(_.vertices).filter(null !=)
        vertices.filter(vSet contains).map(v => (v.nr, m.addVertex(v.pos).nr))
      }
      val tMap = Map(0 -> 0) ++ {
    	val tSet = Set() ++ faces.flatMap(_.textureVertices).filter(null !=)
        textureVertices.filter(tSet contains)
          .map(t => (t.nr, m.addTextureVertex(t.pos).nr))
      }
      val nMap = Map(0 -> 0) ++ {
    	val nSet = Set() ++ faces.flatMap(_.normals).filter(null !=)
        normals.filter(nSet contains).map(n => (n.nr, m.addNormal(n.value).nr))
      }
      for (f <- faces) {
        val cs = f.vertexChambers.toSeq
        val vs = cs.map(c => vMap(c.vertexNr))
        val vt = cs.map(c => tMap(c.tVertexNr))
        val vn = cs.map(c => nMap(c.normalNr))
        val g = m.addFace(vs, vt, vn)
        g.material = m.material(f.material.name)
        g.obj = m.obj(f.obj.name)
        g.group = m.group(f.group.name)
        g.smoothingGroup = f.smoothingGroup
      }
      m
    }
  
  def withDonorData(donor: Mesh, f: Map[Chamber, Chamber] => boolean) = {
    val result = clone
    val originals = result.components
    
    for (comp <- donor.components) {
      send("Matching donor component with %d chambers..."
           format (comp.chambers.size))
      var dist = Double.MaxValue
      var map: Map[Chamber, Chamber] = null
      var image: Component = null
      try {
        for (c <- originals) {
          val candidate = closest(allMatches(comp, c), _.vertex)
          if (candidate != null) {
            val d = distance(candidate, _.vertex)
            if (d < dist) {
              dist = d
              map = candidate
              image = c
            }
          }
        }
        if (map == null) send("No match found.")
      } catch {
        case ex: Throwable =>
          send("Error while matching! Skipping this component.\n"
               + ex.getMessage + "\n" + ex.getStackTraceString)
      }
      if (map != null) {
        send("Match found. Applying donor data...")
        f(map)
      }
    }
    result
  }
  
  def withMorphApplied(donor: Mesh) =
    withDonorData(donor, map => {
      for ((c, d) <- map) d.vertex.pos = c.vertex.pos
      true
    })
  
  def withMorphAtStrength(donor: Mesh, f: Double) =
    withDonorData(donor, map => {
      var seen = new HashSet[Vertex]
      for ((c, d) <- map) {
        if (!seen(d.vertex)) {
          d.vertex.pos = (1 - f) * d.vertex.pos + f * c.vertex.pos
          seen += d.vertex
        }
      }
      true
    })
  
  def withGroupingFrom(donor: Mesh, flags: String) =
    withDonorData(donor, map => {
      for ((c, d) <- map) {
        val f = c.face
        val g = d.face
        if (f != null) {
          if (flags.contains('g')) g.group = d.mesh.group(f.group.name)
          if (flags.contains('m')) g.material = d.mesh.material(f.material.name)
          if (flags.contains('s')) g.smoothingGroup = f.smoothingGroup
        }
      }
      true
    })
  
  def withUVsFrom(donor: Mesh) =
    withDonorData(donor, map => {
      val tMap = new HashMap[TextureVertex, TextureVertex]
      for ((c, d) <- map) {
        val t = c.tVertex
        if (t != null) {
        	if (!tMap.contains(t)) tMap(t) = d.mesh.addTextureVertex(t.pos)
        	d.tVertex = tMap(t)
        }
      }
      true
    })
  
  def subdivision = {
    // -- create a new empty mesh
    val subD = new Mesh
    
    // -- copy the original vertices, texture vertices and normals
    for (v <- vertices) subD.addVertex(v.pos)
    for (t <- textureVertices) subD.addTextureVertex(t.pos)
    for (n <- normals) subD.addNormal(n.value)
    
    // -- create edge centers
    val ch2ev = new HashMap[Chamber, Vertex]
    for (c <- _edges.values) {
      val z = subD.addVertex((c.start.pos + c.end.pos) / 2)
      for (d <- List(c, c.s0, c.s2, c.s0.s2)) ch2ev(d) = z
    }
    
    // -- interpolate texture coordinates along vertices
    val ch2etnr = new HashMap[Chamber, Int]
    for (c <- chambers if !ch2etnr.contains(c)) {
      val t1 = c.tVertex
      val t2 = c.s0.tVertex
      if (t1 != null && t2 != null) {
        val z = subD.addTextureVertex((t1.pos + t2.pos) / 2)
        for (d <- List(c, c.s0)) ch2etnr(d) = z.nr
      }
    }
    
    // -- create face centers and subdivision faces
    for (f <- faces) {
      val n = f.degree
      val z = subD.addVertex(f.vertices.sum(_.pos) / n).nr
      val tz =
        if (f.textureVertices.forall(null !=))
          subD.addTextureVertex(f.textureVertices.sum(_.pos) / n).nr
        else 0
      for (c <- f.vertexChambers) {
        val t = c.tVertexNr
        val n = c.normalNr
        val g = subD.addFace(
          List(c.vertexNr,  ch2ev(c).nr, z, ch2ev(c.s1).nr),
          List(t, ch2etnr.getOrElse(c, 0), tz, ch2etnr.getOrElse(c.s1, 0)),
          List(n, n, n, n))
        g.obj      = subD.obj(f.obj.name)
        g.material = subD.material(f.material.name)
        g.group    = subD.group(f.group.name)
        g.smoothingGroup = f.smoothingGroup
      }
    }
    
    // -- fill holes and flag border vertices
    subD.fixHoles
    val onBorder = new HashSet[Vertex]
    for (c <- subD.hardChambers) onBorder += c.vertex
    
    // -- adjust positions of edge centers
    for (c <- _edges.values; val z = ch2ev(c) if !onBorder(z)) {
      val z = ch2ev(c)
      if (z.degree != 4) error("bad new vertex degree in subdivision()")
      z.pos = z.cellChambers.sum(_.s0.vertex.pos) / 4
    }
    
    // -- adjust positions of (copied) original non-border vertices
    for (n <- 1 to numberOfVertices; val v = subD.vertex(n) if !onBorder(v)) {
      val k = v.degree
      val cs = v.cellChambers.toSeq
      v.pos = (((k - 3) * v.pos
                + 4 * cs.sum(_.s0.vertex.pos) / k
                - cs.sum(_.s0.s1.s0.vertex.pos) / k) / k)
    }
    
    // -- do the same for border vertices
    val hard = new HashSet[Chamber]; hard ++ subD.hardChambers
    for (v <- onBorder if v.nr <= numberOfVertices) {
      val breaks = v.cellChambers.filter(hard).toSeq
      if (breaks.size == 2)
        v.pos =
          (breaks(0).s0.vertex.pos + breaks(1).s0.vertex.pos + 2 * v.pos) / 4
    }
    
    // -- return the result
    subD.mtllib ++ mtllib
    subD
  }

  def classifyForCoarsening(seed: Vertex): VertexClassification = {
    val wasVertex     = new HashSet[Vertex]
    val wasEdgeCenter = new HashSet[Vertex]
    val wasFaceCenter = new HashSet[Vertex]

    val onBorder = new HashSet[Vertex]
    for (h <- holes; v <- h.vertices) onBorder += v

    val queue = new Queue[Vertex]
    queue     += seed
    wasVertex += seed
    while (queue.length > 0) {
      val v = queue.dequeue
      for (c <- v.cellChambers) {
        val w = c.s0.vertex
        if (wasVertex(w) || wasFaceCenter(w)) return null
        if (onBorder(w)) {
          if (!onBorder(v) || w.degree != 3) return null
        } else {
          if (w.degree != 4) return null
        }
        wasEdgeCenter += w
        for (d <- List(c.s0.s1.s0, c.s2.s0.s1.s0)) {
          val u = d.vertex
          if (wasEdgeCenter(u)) return null
          if (onBorder(u)) {
            if (!onBorder(w)) return null
            if (!wasVertex(u)) {
              queue     += u
              wasVertex += u
            }
          } else {
            if (wasVertex(u)) return null
            wasFaceCenter += u
          }
        }
        if (!onBorder(w)) {
          val u = c.s0.s1.s2.s1.s0.vertex
          if (wasEdgeCenter(u) || wasFaceCenter(u)) return null
          if (!wasVertex(u)) {
            queue     += u
            wasVertex += u
          }
        }
      }
    }
    VertexClassification(wasVertex, wasEdgeCenter, wasFaceCenter)
  }
  
  def coarsening: Mesh = {
    send("  classifying vertices...")
    val vc = new HashMap[Component, VertexClassification]
    for (p <- components) {
      var cost = Int.MaxValue
      var best: VertexClassification = null
      for (c <- p.coarseningClassifications)
        if (c.cost < cost) {
          cost = c.cost
          best = c
        }
      if (best == null) send("component cannot be coarsened")
      vc(p) = best
    }
    coarsening(vc)
  }
  
  def coarsening(vc: Component => VertexClassification) = {
    //TODO resolve conflicts (texture vertices, materials, groups)

    // -- warning messages
    val messages = new HashSet[String]
    
    // -- chambers and vertices on mesh or smoothing group borders
    send("  finding borders...")
    val hard     = new HashSet[Chamber]; hard     ++ hardChambers
    val onBorder = new HashSet[Vertex] ; onBorder ++ hard.map(_.vertex)

    // -- vertices for which the final position has been computed
    val done = new HashSet[Vertex]

    // -- initialize the new mesh
    send("  initializing new mesh...")
    val m = new Mesh
    
    // -- define how old vertices map to new ones
    send("  defining maps...")
    val mapV = new LazyMap((i: Int) => {
      val v = vertex(i)
      val k  = v.degree
      val cs = v.cellChambers.toSeq
      val w = if (onBorder(v)) {
        done += v
        val breaks = cs.filter(hard).toSeq
        if (breaks.size == 2) m.addVertex(
            v.pos * 2 - (breaks(0).s0.vertex.pos + breaks(1).s0.vertex.pos) / 2)
        else
          m.addVertex(v.pos)
      } else if (k != 3) {
        done += v
        m.addVertex((v.pos * k + (cs.sum(_.s0.s1.s0.vertex.pos)
                                  - 4 * cs.sum(_.s0.vertex.pos)) / k) / (k - 3))
      } else {
        m.addVertex(v.pos)
      }
      w.nr
    })
    
    // -- do the same for texture vertices and normals
    val mapT = new LazyMap((t: Int) =>
      if (t != 0) m.addTextureVertex(textureVertex(t).pos).nr else 0)
    val mapN = new LazyMap((n: Int) =>
      if (n != 0) m.addNormal(normal(n).value).nr else 0)
    
    // -- create the faces of the new mesh along with necessary vertices etc.
    send("  making faces...")
    for (p <- components if vc(p) != null; f <- vc(p).wasFaceCenter) {
      val cs = f.cellChambers.toSeq
      val vs = cs.map(c => mapV(c.s0.s1.s0.vertexNr))
      val vt = cs.map(c => mapT(c.s0.s1.s0.tVertexNr))
      val vn = cs.map(c => mapN(c.s0.s1.s0.normalNr))
      val face = m.addFace(vs.reverse, vt.reverse, vn.reverse)
      
      val objects   = new HashMap[Object, Int]
      val groups    = new HashMap[Group, Int]
      val materials = new HashMap[Material, Int]
      val sgroups   = new HashMap[Int, Int]
      var badUVs    = false
      for (c <- cs) {
        val face = c.face
        objects(face.obj) = objects.getOrElse(face.obj, 0) + 1
        groups(face.group) = groups.getOrElse(face.group, 0) + 1
        materials(face.material) = materials.getOrElse(face.material, 0) + 1
        sgroups(face.smoothingGroup) =
          sgroups.getOrElse(face.smoothingGroup, 0) + 1
        if (c.tVertex != c.s2.tVertex || c.s0.tVertex != c.s0.s2.tVertex)
          badUVs = true
      }
      if (badUVs)
          messages += ("Inconsistent texture vertices: %s" format groups)
      if (objects.size > 1)
        messages += ("Inconsistent objects: %s" format objects)
      if (groups.size > 1)
        messages += ("Inconsistent grouping: %s" format groups)
      if (materials.size > 1)
        messages += ("Inconsistent materials: %s" format materials)
      if (sgroups.size > 1) messages += "Inconsistent smoothing groups."
      face.obj = objects.keys.next
      face.group = groups.keys.next
      face.material = materials.keys.next
      face.smoothingGroup = sgroups.keys.next
    }
    
    // -- print out the accumulated warning messages
    for (msg <- messages) send("Warning: " + msg)
    
    // -- compute final positions for the 3-pole vertices in waves
    send("  handling 3-poles...")
    val nextWave = new HashSet[Vertex]
    nextWave ++ done
    while (nextWave.size > 0) {
      val thisWave = new HashSet[Vertex]
      thisWave ++ nextWave
      nextWave.clear
      for (v <- thisWave) {
        val vnew = m.vertex(mapV(v.nr))
        var p = Vec3(0, 0, 0)
        var n = 0
        for (c <- v.cellChambers if !hard(c)) {
          val w = c.s0.s1.s2.s1.s0.vertex
          if (done(w)) {
            p += (4 * c.s0.vertex.pos - m.vertex(mapV(w.nr)).pos
                  - c.s0.s1.s0.vertex.pos - c.s0.s2.s1.s0.vertex.pos)
            n += 1
          } else {
            nextWave += w
          }
        }
        if (!done(v)) vnew.pos = p / n
      }
      done ++ thisWave
    }
    
    // -- return the new mesh
    m.mtllib ++ mtllib
    m
  }
}
