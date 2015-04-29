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

  def dontintersect(region: SpaceRegion): Boolean = {
    var exclude = false
    var dim = 1
    while (dim <= this.dim && !exclude) {
      exclude = !containsFromDimention(region.lb, dim) && !containsFromDimention(region.ub, dim)
      dim += 1
    }
    exclude
  }

  def intersect(region: SpaceRegion): Boolean = !dontintersect(region)

  def shrink(pivot: SpacePoint, dim: Int): (SpaceRegion, SpaceRegion) = {
    (SpaceRegion(lb, ub.copyBut(pivot, dim)), SpaceRegion(lb.copyBut(pivot, dim), ub))
  }

}

object SpaceRegion {
  def apply[A <: SpacePoint](lb: A, ub: A): SpaceRegion = {
    if (lb.dim != ub.dim || !ub.geq(lb)) throw new Exception("ub must have all its coordinate greater than those of lb")
    new SpaceRegion(lb, ub)
  }
}