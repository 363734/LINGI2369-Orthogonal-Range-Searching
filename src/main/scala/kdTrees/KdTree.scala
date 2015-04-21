package kdTrees

/**
 * Abstract class defining the methods used for a KdTree
 */
abstract class KdTree[A](val value: Point[A], val region: SpaceRegion[A])(implicit ord: Ordering[A]) {

  def searchKD(range: SpaceRegion[A]): Set[Point[A]]
  def reportSubtree: Set[Point[A]]

  def getLeftTree(): KdTree[A]
  def getRightTree(): KdTree[A]
}

/**
 * Object KdTree on which the methods will be used
 */
object KdTree {
  /**
   * Constructor of the KdTree : sorts the points and make the tree out of them
   * In : Points in the search space
   * Out : KdTree corresponding
   */
  def apply[A](data: Set[Point[A]], dim: Int)(implicit ord: Ordering[A]): KdTree[A] = {
    // TODO: check all points have dimention dim and unique id
    val sortedPoints = Array.tabulate(dim)(d => data.toArray.sortBy(p => (p.coord(d), p.id)))
    this.buildKdTree(sortedPoints, 0, FullSpace(dim))
  }

  /**
   * Method used to build a KdTree from a set of points
   * In : sorted points, current depth in the tree and search space
   * Out : sub KdTree
   */
  private def buildKdTree[A](sortedPoints: Array[Array[Point[A]]], depth: Int, region: SpaceRegion[A])(implicit ord: Ordering[A]): KdTree[A] = {
    if (sortedPoints(0).length == 1) {
      KdLeaf(sortedPoints(0)(0), sortedPoints(0)(0).getRegion)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(depth)(sublength)
      val pivotCode = (pivot.coord(depth), pivot.id)
      val leftSortedPoints = sortedPoints.map(data => data.filter(d => ord.lteq(d.coord(depth), pivotCode._1) && (d.id <= pivotCode._2)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(ord.lteq(d.coord(depth), pivotCode._1) && (d.id <= pivotCode._2))))
      val newdepth = (depth + 1) % sortedPoints.length
      val (leftRegion, rightRegion) = region.shrink(depth, pivotCode._1)
      KdNode(pivot, region, depth, KdTree.buildKdTree(leftSortedPoints, newdepth, leftRegion), KdTree.buildKdTree(rightSortedPoints, newdepth, rightRegion))
    }
  }

}

/**
 * Class representing an internal node of a KdTree
 */
case class KdNode[A](valueNode: Point[A], regionNode: SpaceRegion[A], val dimNode: Int, val left: KdTree[A], val right: KdTree[A])(implicit ord: Ordering[A]) extends KdTree[A](valueNode, regionNode) {

  def reportSubtree: Set[Point[A]] = {
    left.reportSubtree ++ right.reportSubtree
  }

  def searchKD(range: SpaceRegion[A]): Set[Point[A]] = {
    val leftReported: Set[Point[A]] = (if (range.contains(left.region))
      left.reportSubtree
    else {
      if (range.intersect(left.region))
        left.searchKD(range)
      else
        Set()
    })
    val rightReported: Set[Point[A]] = (if (range.contains(right.region))
      right.reportSubtree
    else {
      if (range.intersect(right.region))
        right.searchKD(range)
      else
        Set()
    })
    leftReported ++ rightReported
  }

  def getLeftTree(): KdTree[A] = left

  def getRightTree(): KdTree[A] = right

}

/**
 * Class representing a leaf of a KdTree
 */
case class KdLeaf[A](valueNode: Point[A], regionLeaf: SpaceRegion[A])(implicit ord: Ordering[A]) extends KdTree[A](valueNode, regionLeaf) {

  def reportSubtree: Set[Point[A]] = {
    Set(value)
  }

  def searchKD(range: SpaceRegion[A]): Set[Point[A]] = {
    if (range.contains(value))
      Set(value)
    else
      Set()
  }

  def getLeftTree(): KdTree[A] = null

  def getRightTree(): KdTree[A] = null

}

/**
 * Class representing a point in the search space
 * In : point ID and coordinates (x,y,...)
 */
case class Point[A](val id: Int, val coordinate: Array[A])(implicit ord: Ordering[A]) {
  val dim = coordinate.length

  /**
   * Returns one of the coordinate of the point (x, y, z...)
   */
  def coord(dim: Int) = { // dim from 1 to this.dim
    coordinate(dim)
  }
  def getRegion: SpaceRegion[A] = {
    val coordinateO: Array[Option[A]] = coordinate.map(Some(_))
    SpaceRegion(coordinateO, coordinateO)
  }
  override def toString: String = {
    "(" + coordinate.mkString(",") + ")"
  }
}

/**
 * Class representing the search space given to a search on the tree
 * In : lower bounds and upper bounds (ex: (0,0,0) (2,2,2) for the cube bound to this points)
 */
case class SpaceRegion[A](val lb: Array[Option[A]], val ub: Array[Option[A]])(implicit ord: Ordering[A]) {

  //TODO : check the tree are build right.. what in case of search space with a same bound?

  def contains(point: Point[A]): Boolean = {
    (0 until point.dim).forall(i => (lb(i).isEmpty || ord.lteq(lb(i).get, point.coord(i))) && (ub(i).isEmpty || ord.lteq(point.coord(i), ub(i).get)))
  }

  def contains(region: SpaceRegion[A]): Boolean = {
    (0 until lb.length).forall(i => ((lb(i).isEmpty) || (!region.lb(i).isEmpty && ord.lteq(lb(i).get, region.lb(i).get))) &&
      ((ub(i).isEmpty) || (!region.ub(i).isEmpty && ord.lteq(region.ub(i).get, ub(i).get))))
  }

  def dontcontains(region: SpaceRegion[A]): Boolean = {
    (0 until lb.length).forall { i =>
      val a = if (!lb(i).isEmpty && !region.ub(i).isEmpty)
        ord.lt(region.ub(i).get, lb(i).get)
      else
        false
      val b = if (!ub(i).isEmpty && !region.lb(i).isEmpty)
        ord.lt(ub(i).get, region.lb(i).get)
      else
        false
      a || b
    }
  }

  def intersect(region: SpaceRegion[A]): Boolean = {
    !dontcontains(region)
  }

  def contains(dim: Int, coordinate: A): Boolean = {
    (lb(dim).isEmpty || ord.lteq(lb(dim).get, coordinate)) && (ub(dim).isEmpty || ord.lteq(coordinate, ub(dim).get))
  }

  def leftcontains(dim: Int, coordinate: A): Boolean = {
    (lb(dim).isEmpty || ord.lteq(lb(dim).get, coordinate))
  }

  def rightcontains(dim: Int, coordinate: A): Boolean = {
    (ub(dim).isEmpty || ord.lteq(coordinate, ub(dim).get))
  }

  def sidecontains(dim: Int, coordinate: A): Boolean = {
    (!ub(dim).isEmpty && ord.lteq(ub(dim).get, coordinate))
  }

  override def clone = {
    SpaceRegion(lb.clone, ub.clone)
  }

  def shrink(dim: Int, value: A): (SpaceRegion[A], SpaceRegion[A]) = {
    val left = this.clone
    val right = this.clone
    left.ub(dim) = Some(value)
    right.lb(dim) = Some(value)
    (left, right)
  }
}
/**
 * Object building the full space of the right number of dimensions
 */
object FullSpace {
  def apply[A](dim: Int)(implicit ord: Ordering[A]): SpaceRegion[A] = {
    val none: Option[A] = None
    SpaceRegion(Array.fill(dim)(none), Array.fill(dim)(none))
  }
}
object test extends App {
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))

  val tree = KdTree(points.toSet, 2)
  println(tree.searchKD(SpaceRegion(Array(Some(1), Some(2)), Array(Some(5), Some(5)))))

}