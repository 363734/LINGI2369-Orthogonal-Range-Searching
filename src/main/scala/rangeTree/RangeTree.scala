package rangeTree

import kdTrees._

/**
 *  Abstract class defining the methods used for a rangeTree
 */
abstract class RangeTree[A](val value: Point[A], val associatedTree: Option[RangeTree[A]], val depth: Int)(implicit ord: Ordering[A]) {

  def reportSubtree: Set[Point[A]]

  def findSplitNode(region: SpaceRegion[A]): RangeTree[A]

  def rangeQuery(region: SpaceRegion[A]): Set[Point[A]] = {
    val splitNode = this.findSplitNode(region)
    splitNode.rangeQueryFromSplitNode(region)
  }

  def rangeQueryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]]
  def rangeQueryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]]
  def rangeQueryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]]

  def getLeftTree(): RangeTree[A]
  def getRightTree(): RangeTree[A]
  def getAssoTree(): Option[RangeTree[A]]
}

/**
 * Object that will be used to build the RangeTree
 */
object RangeTree {
  /**
   * Constructor od the RangeTree
   * In : a set of Points and a Dimention
   * Out : the corresponding rangeTree
   */
  def apply[A](data: Set[Point[A]], dim: Int)(implicit ord: Ordering[A]): RangeTree[A] = {
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortBy(p => (p.coord(d), p.id)))
    this.buildRangeTree(sortedPoints, 0, dim)
  }

  /**
   * Method used to build the rangeTree from a sorted set of points
   * In : list of sorted points, a depth (dimension corresponding to the rangeTree) and the total number of dimensions
   * Out : The rangeTree corresponding
   */
  def buildRangeTree[A](sortedPoints: List[Array[Point[A]]], depth: Int, dim: Int)(implicit ord: Ordering[A]): RangeTree[A] = {
    val associatedTree = if (depth == dim - 1)
      None
    else
      Some(RangeTree.buildRangeTree(sortedPoints.tail, depth + 1, dim))

    if (sortedPoints(0).length == 1) {
      RangeLeaf(sortedPoints(0)(0), associatedTree, depth)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)
      val pivotCode = (pivot.coord(depth), pivot.id)
      val leftSortedPoints = sortedPoints.map(data => data.filter(d => ord.lteq(d.coord(depth), pivotCode._1) && (d.id <= pivotCode._2)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(ord.lteq(d.coord(depth), pivotCode._1) && (d.id <= pivotCode._2))))
      RangeNode(pivot, associatedTree, depth, RangeTree.buildRangeTree(leftSortedPoints, depth, dim), RangeTree.buildRangeTree(rightSortedPoints, depth, dim))
    }
  }
}

/**
 * Class representing an internal node of a rangeTree
 */
case class RangeNode[A](valueNode: Point[A], assoTree: Option[RangeTree[A]], depthNode: Int, val left: RangeTree[A], val right: RangeTree[A])(implicit ord: Ordering[A]) extends RangeTree[A](valueNode, assoTree, depthNode) {

  def reportSubtree: Set[Point[A]] = {
    left.reportSubtree ++ right.reportSubtree
  }

  def findSplitNode(region: SpaceRegion[A]): RangeTree[A] = {
    if (region.contains(depth, value.coord(depth)))
      this
    else {
      if (region.sidecontains(depth, value.coord(depth))) { // ?? +-?
        left.findSplitNode(region)
      } else {
        right.findSplitNode(region)
      }
    }
  }

  def rangeQueryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    left.rangeQueryFromSplitNodeLeft(region) ++ right.rangeQueryFromSplitNodeRight(region)
  }

  def rangeQueryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.leftcontains(depth, value.coord(depth))) {
      (if (associatedTree.isEmpty)
        right.reportSubtree
      else
        associatedTree.get.rangeQuery(region)) ++ left.rangeQueryFromSplitNodeLeft(region)
    } else {
      right.rangeQueryFromSplitNodeLeft(region) ++ left.rangeQueryFromSplitNodeLeft(region)
    }
  }

  def rangeQueryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.rightcontains(depth, value.coord(depth))) {
      (if (associatedTree.isEmpty)
        left.reportSubtree
      else
        associatedTree.get.rangeQuery(region)) ++ right.rangeQueryFromSplitNodeRight(region)
    } else {
      left.rangeQueryFromSplitNodeRight(region) ++ right.rangeQueryFromSplitNodeRight(region)
    }
  }

  def getLeftTree(): RangeTree[A] = left

  def getRightTree(): RangeTree[A] = right

  def getAssoTree(): Option[RangeTree[A]] = assoTree
}

/**
 * Class representing an leaf of a rangeTree
 */
case class RangeLeaf[A](valueLeaf: Point[A], assoTree: Option[RangeTree[A]], depthLeaf: Int)(implicit ord: Ordering[A]) extends RangeTree[A](valueLeaf, assoTree, depthLeaf) {

  def reportSubtree: Set[Point[A]] = {
    Set(value)
  }

  def findSplitNode(region: SpaceRegion[A]): RangeTree[A] = {
    this
  }

  def rangeQueryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }
  def rangeQueryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }
  def rangeQueryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): RangeTree[A] = null

  def getRightTree(): RangeTree[A] = null

  def getAssoTree(): Option[RangeTree[A]] = assoTree
}

object testt extends App {
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))

  val tree = RangeTree(points.toSet, 2)
  println(tree.rangeQuery(SpaceRegion(Array(Some(1), Some(2)), Array(Some(5), Some(5)))))

}
