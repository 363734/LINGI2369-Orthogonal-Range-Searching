package fractionalCascading

import space._

/**
 *  Abstract class defining the methods used for a rangeTree using fractional cascading
 */
abstract class FractionalCascading {

  /**
   * Performs a query on this node and returns the set of points in the search space
   */
  def query(region: SpaceRegion): Set[SpacePoint]

  /**
   * Getter associated to different structures (mainly used for tests)
   */
  def getLeftTree(): FractionalCascading
  def getRightTree(): FractionalCascading
  def getAssoTree(): FractionalCascading
  def getValue(): SpacePoint
  def getDepth(): Int
}

/**
 * Abstract class defining the methods used for a rangeTree using fractional cascading for a non-terminal tree
 */
abstract class FractionalTree(val value: SpacePoint,
                              val depth: Int) extends FractionalCascading {

  /**
   *  Returns the split node from the search
   */
  def findSplitNode(region: SpaceRegion): FractionalTree

  def query(region: SpaceRegion): Set[SpacePoint] = {
    val splitNode = this.findSplitNode(region)
    splitNode.queryFromSplitNode(region)
  }

  /**
   * Group of functions the query on this node :
   * d1RangeQueryFromSplitNode : on the current Node
   * d1RangeQueryFromSplitNodeLeft : on a left childNode
   * d1RangeQueryFromSplitNodeRight : on a right childNode
   */
  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint]
  def queryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint]
  def queryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint]
}

/**
 * Object that will be used to build the RangeTree with fractional cascading
 */
object FractionalTree {
  /**
   * Constructor of the RangeTree with fractional cascading
   * In : a set of Points and a dimension
   * Out : the corresponding rangeTree with fractional cascading
   */
  def apply(data: Set[SpacePoint], dim: Int): FractionalCascading = {
    if (dim < 2) throw new Exception("Fractional Cascading don't work with dim < 2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortWith((p1, p2) => !p1.geqIndex(p2, d + 1)))
    this.buildFCTree(sortedPoints, 1, dim)
  }

  /**
   * Method used to build the rangeTree with fractional cascading from a sorted set of points
   * In : list of sorted points, a depth (dimension corresponding to the current rangeTree) and the total number of dimensions
   * Out : The rangeTree with fractional cascading corresponding
   */
  def buildFCTree(sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): FractionalCascading = {
    if (depth == dim - 1) {
      FractionalLastTree.buildFCLastTree(sortedPoints, depth, dim)
    } else {
      FractionalTree.buildFCTreeSameDepth(sortedPoints, depth, dim)
    }
  }

  /**
   * Method used to build the fractional cascading structure and launch the one for the next dimension
   * In : list of sorted points, a depth (dimension corresponding to the current rangeTree) and the total number of dimensions
   * Out : The fractional cascading structure corresponding
   */
  def buildFCTreeSameDepth[A](sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): FractionalTree = {
    if (sortedPoints(0).length == 1) {
      FractionalTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val associatedTree = FractionalTree.buildFCTree(sortedPoints.tail, depth + 1, dim)

      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => !d.geqIndex(pivot, depth) || (d == pivot)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(!d.geqIndex(pivot, depth) || (d == pivot))))

      FractionalTreeNode(pivot, associatedTree, FractionalTree.buildFCTreeSameDepth(leftSortedPoints, depth, dim), FractionalTree.buildFCTreeSameDepth(rightSortedPoints, depth, dim), depth)
    }
  }

}

/**
 * Class representing an internal node of a rangeTree with fractional cascading
 */
case class FractionalTreeNode(valueNode: SpacePoint,
                              val associatedTree: FractionalCascading,
                              val left: FractionalTree,
                              val right: FractionalTree,
                              depthNode: Int) extends FractionalTree(valueNode, depthNode) {

  def findSplitNode(region: SpaceRegion): FractionalTree = {
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

  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    left.queryFromSplitNodeLeft(region) ++ right.queryFromSplitNodeRight(region)
  }

  def queryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint] = {
    if (region.leftcontains(value, depth)) {
      associatedTree.query(region) ++ left.queryFromSplitNodeLeft(region)
    } else {
      right.queryFromSplitNodeLeft(region)
    }
  }

  def queryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint] = {
    if (region.rightcontains(value, depth)) {
      associatedTree.query(region) ++ right.queryFromSplitNodeRight(region)
    } else {
      left.queryFromSplitNodeRight(region)
    }
  }

  def getLeftTree(): FractionalTree = left

  def getRightTree(): FractionalTree = right

  def getAssoTree(): FractionalCascading = associatedTree

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}

/**
 * Class representing a leaf node of a rangeTree with fractional cascading
 */
case class FractionalTreeLeaf(valueLeaf: SpacePoint,
                              depthLeaf: Int) extends FractionalTree(valueLeaf, depthLeaf) {

  def findSplitNode(region: SpaceRegion): FractionalTree = {
    this
  }

  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint] = {
    reportIfIn(region)
  }

  def reportIfIn(region: SpaceRegion): Set[SpacePoint] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): FractionalTree = null

  def getRightTree(): FractionalTree = null

  def getAssoTree(): FractionalCascading = null

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}