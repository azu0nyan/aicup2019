import model.{Vec2Double, Vec2Float}

import scala.language.implicitConversions
import scala.util.Random

package object m {

  implicit class VtoV2(v2:Vec2Double){
    def tv:V2 = v2DToV2(v2)
  }
  implicit def v2ToV2D(v2: V2):Vec2Double = Vec2Double(v2.x, v2.y)
  implicit def v2ToV2F(v2: V2):Vec2Float = Vec2Float(v2.x.toFloat, v2.y.toFloat)
  implicit def v2DToV2(v2: Vec2Double):V2 = V2(v2.x, v2.y)


  implicit class Power(val s: Double) extends AnyVal {
    def ^^(p: Double): Double = Math.pow(s, p)

    def squared: Double = s * s

    def cubed: Double = s * s * s

    def tesseracted: Double = s * s * s * s
  }

  val percision = 0.0001

  implicit class WithAlmostEquals(val d: Double) extends AnyVal {
    def ~>=(d2: Double): Boolean = (d > d2) || (this ~= d2)

    def ~>(d2: Double): Boolean = (d > d2) && (this !~= d2)

    def ~<(d2: Double): Boolean = (d < d2) && (this !~= d2)

    def ~<=(d2: Double): Boolean = (d < d2) || (this ~= d2)

    def ~=(d2: Double): Boolean = (d - d2).abs <= percision

    def !~=(d2: Double): Boolean = !(this ~= d2)

    def inc: Double = d + 1
  }
  
  
  object V2 {
    def apply(s:Double): V2 = from(s, s)

    def from(x:Double, y:Double) :V2 = V2(x, y)

    implicit def pairToV2(p: (Double, Double)): V2 = V2(p._1, p._2)

    implicit def fToV2(f: Double): V2 = V2(f, f)



    //@ENGINE CONVERSION


    //implicit def fromVector2D(v: Vector2D): V2 = V2(v.getX.toFloat, v.getY.toFloat)

    //  implicit def toVector2d(v: V2): Vector2D = new Vector2D(v.x, v.y)


    val ox: V2 = V2(1, 0)

    val oy: V2 = V2(0, 1)

    val ZERO : V2= V2(0, 0)




  }

  case class V2(x: Double, y: Double) {


    def apply(i: Int): Double = i match {
      case 0 => x
      case 1 => y
      case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
    }

    def xInt:Int = x.toInt

    def yInt:Int = y.toInt

    def unary_- : V2 = opposite

    def opposite: V2 = V2(-x, -y)

    def +(v: V2): V2 = V2(x + v.x, y + v.y)

    def -(v: V2): V2 = V2(x - v.x, y - v.y)

    def *(v: V2): V2 = V2(x * v.x, y * v.y)

    def *(s:Double): V2 = V2(x * s, y * s)

    def /(v: V2): V2 = V2(x / v.x, y / v.y)

    def -(): V2 = V2(-x, -y)

    def **(v: V2): Double = x * v.x + y * v.y

    def det(v: V2): Double = x * v.y - y * v.x

    def ~=(v: V2): Boolean = (x ~= v.x) && (y ~= v.y)

    def normalize: V2 = if (length == 0) {
      V2(0, 0)
    } else {
      /(length)
    }

    def angle(v: V2): Double = (math.atan2(v.y, v.x) - math.atan2(y, x))

    def distance(v: V2): Double = (this - v).length

    def rotate90CW:V2 = V2(-y, x)

    def rotate90CCW:V2 = V2(y, -x)

    def rotate(a: Double):V2 = V2(x * cos(a) - y * sin(a), x * sin(a) + y * cos(a))

    def rotateAroundPoint(rotation: Double, point: V2): V2 = (this - point).rotate(rotation) + point

    def scaleAroundPoint(scale: Double, point: V2): V2 = (this - point) * scale + point

    def lengthSquared:Double = this ** this

    def length: Double = math.hypot(x, y)

    override def toString: String = s"""V2($x, $y)"""


    def toSeq: Seq[Double] = Seq(x, y)



    def toIntV2: IntV2 = IntV2(x.toInt, y.toInt)

    def collinear(other: V2): Boolean = (normalize ~= other.normalize) || (normalize ~= other.normalize.opposite)
  }

  class UnitV2(v: V2) extends V2(v.normalize.x, v.normalize.y)

  val bigValue: Double = math.pow(2, 32).toFloat

  @inline def atan2(y: Double, x: Double): Double = math.atan2(y, x)

  @inline def sqrt(lengthSquared: Double): Double = math.sqrt(lengthSquared)

  @inline def exp(v: Double): Double = math.exp(v)

  /** natural logarithm  */
  @inline def log_e(v: Double): Double = math.log(v)

  @inline def log2(v: Double): Double = logab(v, 2)

  /** a-value b-base, changing base to e => log_b(a) = log_e(a)/log_e(b) */
  @inline def logab(value: Double, base: Double): Double = log_e(value) / log_e(base)

  @inline def asin(v: Double): Double = math.asin(v)

  @inline def acos(v: Double): Double = math.acos(v)

  @inline def sin(v: Double): Double = math.sin(v) //FastMath.sin(v)

  @inline def cos(v: Double): Double = math.cos(v) //FastMath.cos(v)

  @inline def min(a: Double, b: Double): Double = math.min(a, b)

  @inline def max(a: Double, b: Double): Double = math.max(a, b)

  @inline def floor(s: Double): Double = math.floor(s)

  @inline def ceil(s: Double): Double = math.ceil(s)

  @inline def abs(s: Double): Double = math.abs(s)

  @inline def pow(a: Double, b: Double): Double = math.pow(a, b)

  def min(vals: Double*): Double = vals.reduce(min)

  def max(vals: Double*): Double = vals.reduce(max)

  /** return number from [0, length) */
  def circullarIndex(element: Int, length: Int): Int = {
    if (length == 0) return 0
    if (element >= 0) {
      element % length
    } else {
      (element % length) + length
    }
  }

  //Rands

  def randInRange(a: Double, b: Double, r: Random = new Random()): Double = {
    val min = scala.math.min(a, b)
    val max = scala.math.max(a, b)
    val range = max - min
    r.nextDouble() * range + min
  }

  /**
   * @param a from inclusive
   * @param b to inclusive
   */
  def randIntInRange(a: Int, b: Int, r: Random = new Random()): Int = {
    val min: Int = scala.math.min(a, b)
    val max: Int = scala.math.max(a, b)
    val range = max - min
    r.nextInt(max - min + 1) + min
  }


  def lerp(x1: Double, x1Value: Double, x2: Double, x2Value: Double, point: Double): Double = {
    val diff = x2 - x1
    if (diff == 0f) x1Value
    else x1Value * (x2 - point) / diff + x2Value * (point - x1) / diff
  }

  def lerpUnit(f0: Double, f1: Double, x: Double): Double = f0 * x + f1 * (1 - x)

  def bilerpUnit(x1: Double, y1: Double,
                 x2: Double, y2: Double,
                 q11: Double, q12: Double,
                 q21: Double, q22: Double,
                 x: Double, y: Double): Double = {
    val fxy1 = lerp(x1, q11, x2, q21, x)
    val fxy2 = lerp(x1, q12, x2, q22, x)
    lerp(y1, fxy1, y2, fxy2, y)
  }

  def bilerpUnit(f00: Double, f01: Double,
                 f10: Double, f11: Double,
                 x: Double, y: Double
                ): Double = f00 * (1 - x) * (1 - y) + f01 * (1 - x) * y + f10 * x * (1 - y) + f11 * x * y


  def fifthFade(t: Double): Double = (6 * t * t * t * t * t) - (15 * t * t * t * t) + (10 * t * t * t)

  def fastfloor(x: Double): Int = if (x > 0) x.toInt
  else x.toInt - 1

  object IntV2 {
    //  implicit def toV2(v:IntV2):V2 = v.toV2

    implicit def toIntV2(x: Int): IntV2 = IntV2(x, x)

    implicit def toIntV2(x: (Int, Int)): IntV2 = IntV2(x._1, x._2)

    implicit def fromIntV2(x: IntV2): (Int,Int) = (x.i, x.j)
  }

  val dleft:IntV2 = IntV2(-1, 0)
  val dright:IntV2 = IntV2(1, 0)

  val dtop:IntV2 = IntV2(0, -1)
  val dbottom:IntV2 = IntV2(0, 1)
  case class IntV2(i: Int, j: Int) {
    def x:Int  = i

    def y:Int  = j


    def opposite: IntV2 = IntV2(-i, -j)

    def +(v: IntV2) = IntV2(i + v.i, j + v.j)

    def -(v: IntV2) = IntV2(i - v.i, j - v.j)

    def *(v: IntV2) = IntV2(i * v.i, j * v.j)

    def /(v: IntV2) = IntV2(i / v.i, j / v.j)

    def clampCircullar(resolution: IntV2): IntV2 = IntV2(m.circullarIndex(i, resolution.i), m.circullarIndex(j, resolution.j))

    def toV2: V2 = V2(i, j)

    override def toString: String = s"[$i, $j]"
  }
}
