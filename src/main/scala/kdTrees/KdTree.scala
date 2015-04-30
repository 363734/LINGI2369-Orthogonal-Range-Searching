package kdTrees

import space._

/**
 * Abstract class defining the methods used for a KdTree
 */
abstract class KdTree(val value: SpacePoint, val region: SpaceRegion) {

  /**
   * Performs a query on this node and returns the set of points in the search space
   */
  def searchKD(range: SpaceRegion): Set[SpacePoint]

  /**
   * Report all the points in the subtree of the node
   */
  def reportSubtree: Set[SpacePoint]

  /**
   * Getter functions for the sub-trees
   */
  def getLeftTree(): KdTree
  def getRightTree(): KdTree
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
  def apply(data: Set[SpacePoint], dim: Int): KdTree = {
    // TODO: check all points have dimention dim and unique id
    val sortedPoints = Array.tabulate(dim)(d => data.toArray.sortWith((p1, p2) => !p1.geqIndex(p2, d + 1)))
    this.buildKdTree(sortedPoints, 1, sortedPoints(0)(0).getFullSpace)
  }

  /**
   * Method used to build a KdTree from a set of points
   * In : sorted points, current depth in the tree and search space
   * Out : sub KdTree
   */
  private def buildKdTree(sortedPoints: Array[Array[SpacePoint]], depth: Int, region: SpaceRegion): KdTree = {
    if (sortedPoints(0).length == 1) {
      KdLeaf(sortedPoints(0)(0), sortedPoints(0)(0).getRegion)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(depth - 1)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => !d.geqIndex(pivot, depth) || (d == pivot)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(!d.geqIndex(pivot, depth) || (d == pivot))))

      val newdepth = ((depth) % sortedPoints.length) + 1
      val (leftRegion, rightRegion) = region.shrink(pivot, depth)

      KdNode(pivot, region, depth, KdTree.buildKdTree(leftSortedPoints, newdepth, leftRegion), KdTree.buildKdTree(rightSortedPoints, newdepth, rightRegion))
    }
  }

}

/**
 * Class representing an internal node of a KdTree
 */
case class KdNode(valueNode: SpacePoint, regionNode: SpaceRegion, val dimNode: Int, val left: KdTree, val right: KdTree) extends KdTree(valueNode, regionNode) {

  def reportSubtree: Set[SpacePoint] = {
    left.reportSubtree ++ right.reportSubtree
  }

  def searchKD(range: SpaceRegion): Set[SpacePoint] = {
    val leftReported: Set[SpacePoint] = (if (range.contains(left.region)) {
      left.reportSubtree
    } else {
      if (range.intersect(left.region)) {
        left.searchKD(range)
      } else {
        Set()
      }
    })
    val rightReported: Set[SpacePoint] = (if (range.contains(right.region)) {
      right.reportSubtree
    } else {
      if (range.intersect(right.region)) {
        right.searchKD(range)
      } else {
        Set()
      }
    })
    leftReported ++ rightReported
  }

  def getLeftTree(): KdTree = left

  def getRightTree(): KdTree = right

}

/**
 * Class representing a leaf of a KdTree
 */
case class KdLeaf(valueNode: SpacePoint, regionLeaf: SpaceRegion) extends KdTree(valueNode, regionLeaf) {

  def reportSubtree: Set[SpacePoint] = {
    Set(value)
  }

  def searchKD(range: SpaceRegion): Set[SpacePoint] = {
    if (range.contains(value))
      Set(value)
    else
      Set()
  }

  def getLeftTree(): KdTree = null

  def getRightTree(): KdTree = null

}

/**
 * Obsolete object used for some tests
 */
object test extends App {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))

  val tree = KdTree(points.toSet, 2)
  println(tree.searchKD(SpaceRegion(Point(-1, Array(1, 2)), Point(-1, Array(5, 5)))))

}