package fractionalCascading

import kdTrees.Point
import kdTrees.SpaceRegion

abstract class FractionalCascading[A] {

  def query(region: SpaceRegion[A]): Set[Point[A]]

  def getLeftTree(): FractionalCascading[A]
  def getRightTree(): FractionalCascading[A]
  def getAssoTree(): FractionalCascading[A]
  def getValue(): Point[A]
  def getDepth(): Int
}

abstract class FractionnalTree[A](val value: Point[A],
                                  val depth: Int) extends FractionalCascading[A] {

  def findSplitNode(region: SpaceRegion[A]): FractionnalTree[A]

  def query(region: SpaceRegion[A]): Set[Point[A]] = {
    val splitNode = this.findSplitNode(region)
    splitNode.queryFromSplitNode(region)
  }

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]]
  def queryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]]
  def queryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]]
}

object FractionnalTree {
  def apply[A](data: Set[Point[A]], dim: Int)(implicit ord: Ordering[A]): FractionalCascading[A] = {
    if (dim < 2) throw new Exception("Fractionnal Cascading don't work with dim < 2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortBy(p => (p.coord(d), p.id)))
    this.buildFCTree(sortedPoints, 0, dim)
  }

  def buildFCTree[A](sortedPoints: List[Array[Point[A]]], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionalCascading[A] = {
    if (depth == dim - 2) {
      FractionnalLastTree.buildFCLastTree(sortedPoints, depth, dim)
    } else {
      FractionnalTree.buildFCTreeSameDepth(sortedPoints, depth, dim)
    }
  }

  def buildFCTreeSameDepth[A](sortedPoints: List[Array[Point[A]]], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionnalTree[A] = {
    if (sortedPoints(0).length == 1) {
      FractionnalTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val associatedTree = FractionnalTree.buildFCTree(sortedPoints.tail, depth + 1, dim)
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)
      val pivotCode = (pivot.coord(depth), pivot.id)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => ord.lt(d.coord(depth), pivotCode._1) || ((ord.equiv(d.coord(depth), pivotCode._1) && d.id <= pivotCode._2))))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !((ord.lt(d.coord(depth), pivotCode._1) || ((ord.equiv(d.coord(depth), pivotCode._1) && d.id <= pivotCode._2))))))
      FractionnalTreeNode(pivot, associatedTree, FractionnalTree.buildFCTreeSameDepth(leftSortedPoints, depth, dim), FractionnalTree.buildFCTreeSameDepth(rightSortedPoints, depth, dim), depth)
    }
  }

}

case class FractionnalTreeNode[A](valueNode: Point[A],
                                  val associatedTree: FractionalCascading[A],
                                  val left: FractionnalTree[A],
                                  val right: FractionnalTree[A],
                                  depthNode: Int) extends FractionnalTree[A](valueNode, depthNode) {

  def findSplitNode(region: SpaceRegion[A]): FractionnalTree[A] = {
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

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    left.queryFromSplitNodeLeft(region) ++ right.queryFromSplitNodeRight(region)
  }

  def queryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.leftcontains(depth, value.coord(depth))) {
      associatedTree.query(region) ++ left.queryFromSplitNodeLeft(region)
    } else {
      right.queryFromSplitNodeLeft(region)
    }
  }

  def queryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.rightcontains(depth, value.coord(depth))) {
      associatedTree.query(region) ++ right.queryFromSplitNodeRight(region)
    } else {
      left.queryFromSplitNodeRight(region)
    }
  }

  def getLeftTree(): FractionnalTree[A] = left

  def getRightTree(): FractionnalTree[A] = right

  def getAssoTree(): FractionalCascading[A] = associatedTree

  def getValue(): Point[A] = value

  def getDepth(): Int = depth
}

case class FractionnalTreeLeaf[A](valueLeaf: Point[A],
                                  depthLeaf: Int) extends FractionnalTree[A](valueLeaf, depthLeaf) {

  def findSplitNode(region: SpaceRegion[A]): FractionnalTree[A] = {
    this
  }

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeLeft(region: SpaceRegion[A]): Set[Point[A]] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeRight(region: SpaceRegion[A]): Set[Point[A]] = {
    reportIfIn(region)
  }

  def reportIfIn(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): FractionnalTree[A] = null

  def getRightTree(): FractionnalTree[A] = null

  def getAssoTree(): FractionalCascading[A] = null

  def getValue(): Point[A] = value

  def getDepth(): Int = depth
}