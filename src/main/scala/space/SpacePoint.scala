package space

import scala.reflect.ClassTag

abstract class SpacePoint {
  val id: Int
  val dim: Int

  /**
   * Return true if (this >= that forall dimension)
   * and tie break with index
   */
  def geqIndex(that: SpacePoint): Boolean = {
    (1 to dim).forall(d => comparePoint(that, d) > 0) || ((1 to dim).forall(d => comparePoint(that, d) == 0) && this.id >= that.id)
  }

  /**
   * Return true if (this >= that) forall dimension
   */
  def geq(that: SpacePoint): Boolean = {
    geqFromDim(that, 1)
  }

  /**
   * Return true if (this >= that) for dimension dim
   * and tie break with index
   */
  def geqIndex(that: SpacePoint, dim: Int): Boolean = {
    comparePoint(that, dim) > 0 || ((comparePoint(that, dim) == 0) && this.id >= that.id)
  }

  /**
   * Return true if (this >= that) for dimension dim
   */
  def geq(that: SpacePoint, dim: Int): Boolean = {
    comparePoint(that, dim) >= 0
  }

  /**
   * Return true if (this >= that) forall dimension
   */
  def geqFromDim(that: SpacePoint, dim: Int): Boolean = {
    (dim to this.dim).forall(d => comparePoint(that, d) >= 0)
  }

  def getRegion: SpaceRegion = SpaceRegion(this, this)

  /**
   * Returns `x` where:
   *   - `x < 0` when `this < that` for the dimension dim
   *   - `x == 0` when `this == that` for the dimension dim
   *   - `x > 0` when  `this > that` for the dimension dim
   * with 1 <= dim <= this.dim
   */
  def comparePoint(that: SpacePoint, dim: Int): Int

  /**
   * Copy the SpacePoint except for the dimension dim where
   * it takes the coordinate of point. The id stays the same
   */
  def copyBut(point: SpacePoint, dim: Int): SpacePoint

  def getFullSpace: SpaceRegion

}

case class Point[A](val id: Int, coordinate: Array[A])(implicit ord: Ordering[A], m: ClassTag[A], domainBound: (A, A)) extends SpacePoint {

  val dim = coordinate.length

  def coord(dim: Int) = coordinate(dim - 1)

  def comparePoint(that: SpacePoint, dim: Int): Int = {
    val thatInstance = that.asInstanceOf[Point[A]]
    ord.compare(this.coord(dim), thatInstance.coord(dim))
  }

  def copyBut(point: SpacePoint, dim: Int): SpacePoint = {
    val thatInstance = point.asInstanceOf[Point[A]]
    val newCoordinate = Array.tabulate(this.dim)(index => (if (index + 1 == dim) { thatInstance.coordinate(index) } else { coordinate(index) }))
    Point(this.id, newCoordinate)
  }

  def getFullSpace: SpaceRegion = {
    val lb = Point(-1, Array.fill(dim)(domainBound._1))
    val ub = Point(-1, Array.fill(dim)(domainBound._2))
    SpaceRegion(lb, ub)
  }

  override def toString: String = {
    "(" + coordinate.mkString(",") + ")"
  }
}
