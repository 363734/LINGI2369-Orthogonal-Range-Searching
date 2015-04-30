package space

class SpaceRegion(val lb: SpacePoint, val ub: SpacePoint) {
  val dim: Int = lb.dim

  def contains(point: SpacePoint): Boolean = {
    point.geq(lb) && ub.geq(point)
  }

  def contains(point: SpacePoint, dim: Int): Boolean = {
    point.geq(lb, dim) && ub.geq(point, dim)
  }

  def containsFromDimention(point: SpacePoint, dim: Int): Boolean = {
    point.geqFromDim(lb, dim) && ub.geqFromDim(point, dim)
  }

  def leftcontains(point: SpacePoint, dim: Int): Boolean = {
    point.geq(lb, dim)
  }

  def rightcontains(point: SpacePoint, dim: Int): Boolean = {
    ub.geq(point, dim)
  }

  def leftOfContains(point: SpacePoint, dim: Int): Boolean = {
    !rightcontains(point, dim)
  }

  def contains(region: SpaceRegion): Boolean = {
    this.contains(region.lb) && this.contains(region.ub)
  }

  def intersect(region: SpaceRegion): Boolean = {
    (1 to dim).forall(d => region.contains(lb, d) || region.contains(ub, d))
  }

  def shrink(pivot: SpacePoint, dim: Int): (SpaceRegion, SpaceRegion) = {
    (SpaceRegion(lb, ub.copyBut(pivot, dim)), SpaceRegion(lb.copyBut(pivot, dim), ub))
  }

  override def toString: String = {
    "[" + lb + " " + ub + "]"
  }
}

object SpaceRegion {
  def apply[A <: SpacePoint](lb: A, ub: A): SpaceRegion = {
    if (lb.dim != ub.dim || !ub.geq(lb)) throw new Exception("ub must have all its coordinate greater than those of lb")
    new SpaceRegion(lb, ub)
  }
}

object test extends App {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  val p1 = Point(-1, Array(0, 0))
  val p2 = Point(-1, Array(2, 2))
  val p3 = Point(-1, Array(3, 3))
  val p4 = Point(-1, Array(4, 4))
  val s1 = SpaceRegion(p1, p3)
  val s2 = SpaceRegion(p2, p4)
  println(s1.contains(s2) == false)
  println(s1.intersect(s2) == true)
  println(s2.intersect(s1) == true)
}