package fractionalCascading

import space._
import scala.collection.mutable.Stack

/**
 * Class that will be used to handle the fractional cascading structure itself
 */
case class ArrayPoints(val arrayPoints: Array[SpacePoint]) {
  val arrayLeft: Array[Int] = Array.fill(arrayPoints.length)(-1)
  val arrayRight: Array[Int] = Array.fill(arrayPoints.length)(-1)

  def length = arrayPoints.length

  override def toString: String = arrayPoints.mkString(" -> ")

  /**
   * Seperates the SpaceRegion into two sub-regions according to the fractional cascading rules
   */
  def split(pivot: SpacePoint, leftNumber: Int): (ArrayPoints, ArrayPoints) = {
    var leftIndex = leftNumber
    val leftArray: Array[SpacePoint] = new Array(leftIndex)
    var rightIndex = arrayPoints.length - leftNumber
    val rightArray: Array[SpacePoint] = new Array(rightIndex)

    for (index <- (0 until arrayPoints.length).reverse) {
      val elem = arrayPoints(index)
      if (!elem.geqIndex(pivot, pivot.dim - 1) || (elem == pivot)) {
        leftIndex -= 1
        leftArray(leftIndex) = elem
      } else {
        rightIndex -= 1
        rightArray(rightIndex) = elem
      }
      arrayLeft(index) = leftIndex
      arrayRight(index) = rightIndex
    }

    (ArrayPoints(leftArray), ArrayPoints(rightArray))
  }

  /**
   * Performs a binary search on the space region and returns the index of the first and last points in it
   */
  def binarySearch(region: SpaceRegion): (Int, Int) = {
    val dim = arrayPoints(0).dim

    val first = binarySearch(region.lb, 0, arrayPoints.length - 1)
    val last = binarySearch(region.ub, 0, arrayPoints.length - 1)

    (first, last)
  }

  /**
   * Performs a binary search on the fractional cascading structure with the points and the given bounds
   */
  def binarySearch(bound: SpacePoint, from: Int, to: Int): Int = {
    if (from == to) {
      from
    } else {
      val dim = arrayPoints(0).dim
      val pivot = (from + to) / 2
      if (arrayPoints(pivot).geq(bound, dim)) {
        binarySearch(bound, from, pivot)
      } else {
        binarySearch(bound, pivot + 1, to)
      }
    }
  }

  /**
   * Report a whole part of the fractional cascading structure
   */
  def reportSlice(from: Int, until: Int): Set[SpacePoint] = {
    arrayPoints.slice(from, until).toSet
  }

  /**
   * Report a single point of the fractional cascading structure
   */
  def reportSinglePoint(index: Int, region: SpaceRegion): Set[SpacePoint] = {
    if (index < arrayPoints.length && region.contains(arrayPoints(index))) {
      Set(arrayPoints(index))
    } else {
      Set()
    }
  }

  /**
   * Go trough the fractional cascading structure to find the left most point to return
   */
  def deepLeft(index: Int) = {
    if (index >= arrayPoints.length)
      arrayPoints.length
    else
      arrayLeft(index)
  }

  /**
   * Go trough the fractional cascading structure to find the right most point to return
   */
  def deepRight(index: Int) = {
    if (index >= arrayPoints.length)
      arrayPoints.length
    else
      arrayRight(index)
  }
}

/**
 * Abstract class defining the methods used for a rangeTree using fractional cascading for a terminal tree
 */
abstract class FractionnalLastTree(val value: SpacePoint,
                                   val depth: Int) extends FractionalCascading {

  /**
   *  Returns the split node from the search
   */
  def query(region: SpaceRegion): Set[SpacePoint] = {
    val splitNode = this.findSplitNode(region)
    splitNode.queryFromSplitNode(region)
  }

  /**
   *  Returns the split node from the search
   */
  def findSplitNode(region: SpaceRegion): FractionnalLastTree

  /**
   * Group of functions the query on this node :
   * d1RangeQueryFromSplitNode : on the current Node
   * d1RangeQueryFromSplitNodeLeft : on a left childNode
   * d1RangeQueryFromSplitNodeRight : on a right childNode
   */
  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint]
  def queryFromSplitNodeLeft(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint]
  def queryFromSplitNodeRight(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint]

  /**
   * Report all the points in the subtree of the node
   */
  def reportSubtree(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint]
}

/**
 * Object that will be used to build the fractional cascading structure for the last dimension
 */
object FractionnalLastTree {
  /**
   * Constructor of the fractional cascading structure for the last dimension
   * In : a set of Points and a dimension
   * Out : the corresponding fractional cascading structure for the last dimension
   */
  def apply(data: Set[SpacePoint], dim: Int): FractionnalLastTree = {
    if (dim != 2) throw new Exception("FractionnalLastTree can only be constructed with d=2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortWith((p1, p2) => !p1.geqIndex(p2, d + 1)))
    this.buildFCLastTree(sortedPoints, 1, dim)
  }

  /**
   * Method used to build the fractional cascading structure for the last dimension
   * In : list of sorted points, a depth (dimension corresponding to the current rangeTree) and the total number of dimensions
   * Out : The fractional cascading structure for the last dimension
   */
  def buildFCLastTree[A](sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): FractionnalLastTree = {
    val initialArrayNode = ArrayPoints(sortedPoints.last)
    this.buildFCLastTree(sortedPoints, initialArrayNode, depth, dim)
  }

  /**
   * Method used to build a fractional cascading structure
   * In : list of sorted points, a depth (dimension corresponding to the current rangeTree) and the total number of dimensions
   * Out : The fractional cascading structure corresponding
   */
  def buildFCLastTree[A](sortedPoints: List[Array[SpacePoint]], arrayNode: ArrayPoints, depth: Int, dim: Int): FractionnalLastTree = {
    if (sortedPoints(0).length == 1) {
      FractionnalLastTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => !d.geqIndex(pivot, pivot.dim - 1) || (d == pivot)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(!d.geqIndex(pivot, pivot.dim - 1) || (d == pivot))))

      val (leftANode, rightANode) = arrayNode.split(pivot, leftSortedPoints(0).length)
      FractionnalLastTreeNode(pivot, arrayNode, FractionnalLastTree.buildFCLastTree(leftSortedPoints, leftANode, depth, dim), FractionnalLastTree.buildFCLastTree(rightSortedPoints, rightANode, depth, dim), depth)
    }
  }
}

/**
 * Class representing an internal node of a rangeTree with fractional cascading at the last depth
 */
case class FractionnalLastTreeNode(valueNode: SpacePoint,
                                   val array: ArrayPoints,
                                   val left: FractionnalLastTree,
                                   val right: FractionnalLastTree,
                                   depthNode: Int) extends FractionnalLastTree(valueNode, depthNode) {

  def findSplitNode(region: SpaceRegion): FractionnalLastTree = {
    if (region.contains(value, depth)) {
      this
    } else {
      if (region.leftOfContains(value, depth)) {
        left.findSplitNode(region)
      } else {
        right.findSplitNode(region)
      }
    }
  }

  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    val (low, up) = array.binarySearch(region)
    left.queryFromSplitNodeLeft(region, array.deepLeft(low), array.deepLeft(up)) ++ right.queryFromSplitNodeRight(region, array.deepRight(low), array.deepRight(up))
  }

  def queryFromSplitNodeLeft(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    if (region.leftcontains(value, depth)) {
      right.reportSubtree(region, array.deepRight(low), array.deepRight(up)) ++
        left.queryFromSplitNodeLeft(region, array.deepLeft(low), array.deepLeft(up))
    } else {
      right.queryFromSplitNodeLeft(region, array.deepRight(low), array.deepRight(up))
    }
  }

  def queryFromSplitNodeRight(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    if (region.rightcontains(value, depth)) {
      left.reportSubtree(region, array.deepLeft(low), array.deepLeft(up)) ++
        right.queryFromSplitNodeRight(region, array.deepRight(low), array.deepRight(up))
    } else {
      left.queryFromSplitNodeRight(region, array.deepLeft(low), array.deepLeft(up))
    }
  }

  def reportSubtree(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    array.reportSlice(low, up) ++ array.reportSinglePoint(up, region)
  }

  def getLeftTree(): FractionnalLastTree = left

  def getRightTree(): FractionnalLastTree = right

  def getAssoTree(): FractionnalLastTree = null

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}

case class FractionnalLastTreeLeaf(valueLeaf: SpacePoint,
                                   depthLeaf: Int) extends FractionnalLastTree(valueLeaf, depthLeaf) {

  def findSplitNode(region: SpaceRegion): FractionnalLastTree = {
    this
  }

  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeLeft(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeRight(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def reportSubtree(region: SpaceRegion, low: Int, up: Int): Set[SpacePoint] = {
    reportIfIn(region)
  }

  def reportIfIn(region: SpaceRegion): Set[SpacePoint] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): FractionnalLastTree = null

  def getRightTree(): FractionnalLastTree = null

  def getAssoTree(): FractionnalLastTree = null

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}

/**
 * Obsolete object used for some tests
 */
object test extends App {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3, 7).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4, 3)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))
  println(points.size)
  val tree = FractionnalLastTree(points.toSet, 2)
  val lb = Point(-1, Array(1, 2))
  val ub = Point(-1, Array(5, 5))
  val queryresult = tree.query(SpaceRegion(lb, ub))
  println(queryresult)
  println(queryresult.size)
}
