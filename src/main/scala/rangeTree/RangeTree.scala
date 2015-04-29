package rangeTree

import space._

/**
 *  Abstract class defining the methods used for a rangeTree
 */
abstract class RangeTree(val value: SpacePoint, val associatedTree: Option[RangeTree], val depth: Int) {

  /**
   * Report all the points in the subtree of the node
   */
  def reportSubtree: Set[SpacePoint]

  /**
   *  Returns the split node from the search
   */
  def findSplitNode(region: SpaceRegion): RangeTree

  /**
   * Performs a query on this node and returns the set of points in the search space
   */
  def rangeQuery(region: SpaceRegion): Set[SpacePoint] = {
    val splitNode = this.findSplitNode(region)
    splitNode.rangeQueryFromSplitNode(region)
  }

  /**
   * Group of functions the query on this node :
   * d1RangeQueryFromSplitNode : on the current Node
   * d1RangeQueryFromSplitNodeLeft : on a left childNode
   * d1RangeQueryFromSplitNodeRight : on a right childNode
   */
  def rangeQueryFromSplitNode(region: SpaceRegion): Set[SpacePoint]
  def rangeQueryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint]
  def rangeQueryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint]

  /**
   * Getter functions for the sub-trees
   */
  def getLeftTree(): RangeTree
  def getRightTree(): RangeTree
  def getAssoTree(): Option[RangeTree]
}

/**
 * Object that will be used to build the RangeTree
 */
object RangeTree {
  /**
   * Constructor of the RangeTree
   * In : a set of Points and a dimension
   * Out : the corresponding rangeTree
   */
  def apply[A](data: Set[SpacePoint], dim: Int): RangeTree = {
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortWith((p1, p2) => !p1.geqIndex(p2, d + 1)))
    this.buildRangeTree(sortedPoints, 1, dim)
  }

  /**
   * Method used to build the rangeTree from a sorted set of points
   * In : list of sorted points, a depth (dimension corresponding to the rangeTree) and the total number of dimensions
   * Out : The rangeTree corresponding
   */
  def buildRangeTree[A](sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): RangeTree = {
    val associatedTree = if (depth == dim)
      None
    else
      Some(RangeTree.buildRangeTree(sortedPoints.tail, depth + 1, dim))

    if (sortedPoints(0).length == 1) {
      RangeLeaf(sortedPoints(0)(0), associatedTree, depth)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => !d.geqIndex(pivot, depth) || (d == pivot)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(!d.geqIndex(pivot, depth) || (d == pivot))))

      RangeNode(pivot, associatedTree, depth, RangeTree.buildRangeTree(leftSortedPoints, depth, dim), RangeTree.buildRangeTree(rightSortedPoints, depth, dim))
    }
  }
}

/**
 * Class representing an internal node of a rangeTree
 */
case class RangeNode(valueNode: SpacePoint, assoTree: Option[RangeTree], depthNode: Int, val left: RangeTree, val right: RangeTree) extends RangeTree(valueNode, assoTree, depthNode) {

  def reportSubtree: Set[SpacePoint] = {
    left.reportSubtree ++ right.reportSubtree
  }

  def findSplitNode(region: SpaceRegion): RangeTree = {
    if (region.contains(value, depth))
      this
    else {
      if (region.leftOfContains(value, depth)) {
        left.findSplitNode(region)
      } else {
        right.findSplitNode(region)
      }
    }
  }

  def rangeQueryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    left.rangeQueryFromSplitNodeLeft(region) ++ right.rangeQueryFromSplitNodeRight(region)
  }

  def rangeQueryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint] = {
    if (region.leftcontains(value, depth)) {
      (if (associatedTree.isEmpty)
        right.reportSubtree
      else
        associatedTree.get.rangeQuery(region)) ++ left.rangeQueryFromSplitNodeLeft(region)
    } else {
      right.rangeQueryFromSplitNodeLeft(region)
    }
  }

  def rangeQueryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint] = {
    if (region.rightcontains(value, depth)) {
      (if (associatedTree.isEmpty)
        left.reportSubtree
      else
        associatedTree.get.rangeQuery(region)) ++ right.rangeQueryFromSplitNodeRight(region)
    } else {
      left.rangeQueryFromSplitNodeRight(region)
    }
  }

  def getLeftTree(): RangeTree = left

  def getRightTree(): RangeTree = right

  def getAssoTree(): Option[RangeTree] = assoTree
}

/**
 * Class representing a leaf of a rangeTree
 */
case class RangeLeaf(valueLeaf: SpacePoint, assoTree: Option[RangeTree], depthLeaf: Int) extends RangeTree(valueLeaf, assoTree, depthLeaf) {

  def reportSubtree: Set[SpacePoint] = {
    Set(value)
  }

  def findSplitNode(region: SpaceRegion): RangeTree = {
    this
  }

  def rangeQueryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def rangeQueryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def rangeQueryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }

  def reportIfIn(region: SpaceRegion): Set[SpacePoint] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): RangeTree = null

  def getRightTree(): RangeTree = null

  def getAssoTree(): Option[RangeTree] = assoTree
}

/**
 * Obsolete object used for some tests
 */
object testt extends App {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))

  val tree = RangeTree(points.toSet, 2)
  println(tree.rangeQuery(SpaceRegion(Point(-1, Array(1, 2)), Point(-1, Array(5, 5)))))

}
