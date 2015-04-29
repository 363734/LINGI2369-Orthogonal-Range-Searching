package fractionalCascading

import space._

abstract class FractionalCascading {

  def query(region: SpaceRegion): Set[SpacePoint]

  def getLeftTree(): FractionalCascading
  def getRightTree(): FractionalCascading
  def getAssoTree(): FractionalCascading
  def getValue(): SpacePoint
  def getDepth(): Int
}

abstract class FractionnalTree(val value: SpacePoint,
  val depth: Int) extends FractionalCascading {

  def findSplitNode(region: SpaceRegion): FractionnalTree

  def query(region: SpaceRegion): Set[SpacePoint] = {
    val splitNode = this.findSplitNode(region)
    splitNode.queryFromSplitNode(region)
  }

  def queryFromSplitNode(region: SpaceRegion): Set[SpacePoint]
  def queryFromSplitNodeLeft(region: SpaceRegion): Set[SpacePoint]
  def queryFromSplitNodeRight(region: SpaceRegion): Set[SpacePoint]
}

object FractionnalTree {
  def apply(data: Set[SpacePoint], dim: Int): FractionalCascading = {
    if (dim < 2) throw new Exception("Fractionnal Cascading don't work with dim < 2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortWith((p1, p2) => !p1.geqIndex(p2, d + 1)))
    this.buildFCTree(sortedPoints, 1, dim)
  }

  def buildFCTree(sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): FractionalCascading = {
    if (depth == dim - 1) {
      FractionnalLastTree.buildFCLastTree(sortedPoints, depth, dim)
    } else {
      FractionnalTree.buildFCTreeSameDepth(sortedPoints, depth, dim)
    }
  }

  def buildFCTreeSameDepth[A](sortedPoints: List[Array[SpacePoint]], depth: Int, dim: Int): FractionnalTree = {
    if (sortedPoints(0).length == 1) {
      FractionnalTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val associatedTree = FractionnalTree.buildFCTree(sortedPoints.tail, depth + 1, dim)

      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => !d.geqIndex(pivot, depth) || (d == pivot)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(!d.geqIndex(pivot, depth) || (d == pivot))))

      FractionnalTreeNode(pivot, associatedTree, FractionnalTree.buildFCTreeSameDepth(leftSortedPoints, depth, dim), FractionnalTree.buildFCTreeSameDepth(rightSortedPoints, depth, dim), depth)
    }
  }

}

case class FractionnalTreeNode(valueNode: SpacePoint,
  val associatedTree: FractionalCascading,
  val left: FractionnalTree,
  val right: FractionnalTree,
  depthNode: Int) extends FractionnalTree(valueNode, depthNode) {

  def findSplitNode(region: SpaceRegion): FractionnalTree = {
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

  def getLeftTree(): FractionnalTree = left

  def getRightTree(): FractionnalTree = right

  def getAssoTree(): FractionalCascading = associatedTree

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}

case class FractionnalTreeLeaf(valueLeaf: SpacePoint,
  depthLeaf: Int) extends FractionnalTree(valueLeaf, depthLeaf) {

  def findSplitNode(region: SpaceRegion): FractionnalTree = {
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

  def getLeftTree(): FractionnalTree = null

  def getRightTree(): FractionnalTree = null

  def getAssoTree(): FractionalCascading = null

  def getValue(): SpacePoint = value

  def getDepth(): Int = depth
}