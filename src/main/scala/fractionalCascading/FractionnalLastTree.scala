package fractionalCascading

import kdTrees.Point
import kdTrees.SpaceRegion
import scala.collection.mutable.Stack

case class ArrayPoints[A](val arrayPoints: Array[Point[A]]) {
  val arrayLeft: Array[Int] = Array.fill(arrayPoints.length)(-1)
  val arrayRight: Array[Int] = Array.fill(arrayPoints.length)(-1)

  def length = arrayPoints.length

  override def toString: String = arrayPoints.mkString(" -> ")

  def split(pivot: Point[A], leftNumber: Int): (ArrayPoints[A], ArrayPoints[A]) = {
    var leftIndex = leftNumber
    val leftArray: Array[Point[A]] = new Array(leftIndex)
    var rightIndex = arrayPoints.length - leftNumber
    val rightArray: Array[Point[A]] = new Array(rightIndex)

    for (index <- (0 until arrayPoints.length).reverse) {
      val elem = arrayPoints(index)
      if (elem.<=(pivot, pivot.dim - 2)) {
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

  def binarySearch(region: SpaceRegion[A]): (Int, Int) = {
    val dim = arrayPoints(0).dim
    val lb = region.lb(dim - 1)
    val ub = region.ub(dim - 1)

    val first = if (lb.isEmpty) {
      0
    } else {
      binarySearch(lb.get, 0, arrayPoints.length - 1)
    }
    val last = if (ub.isEmpty) {
      arrayPoints.length
    } else {
      binarySearch(ub.get, 0, arrayPoints.length - 1)
    }

    (first, last)
  }

  def binarySearch(bound: A, from: Int, to: Int): Int = {
    if (from == to) {
      from
    } else {
      val dim = arrayPoints(0).dim
      val pivot = (from + to) / 2
      if (arrayPoints(pivot).>=(bound, dim - 1)) {
        binarySearch(bound, from, pivot)
      } else {
        binarySearch(bound, pivot + 1, to)
      }
    }
  }

  def reportSlice(from: Int, until: Int): Set[Point[A]] = {
    arrayPoints.slice(from, until).toSet
  }

  def reportSinglePoint(index: Int, region: SpaceRegion[A]): Set[Point[A]] = {
    if (index < arrayPoints.length) {
      val value = arrayPoints(index)
      if (region.contains(value)) {
        Set(value)
      } else {
        Set()
      }
    } else {
      Set()
    }
  }

  def deepLeft(index: Int) = {
    if (index >= arrayPoints.length)
      arrayPoints.length
    else
      arrayLeft(index)
  }

  def deepRight(index: Int) = {
    if (index >= arrayPoints.length)
      arrayPoints.length
    else
      arrayRight(index)
  }
}

object FractionnalLastTree {
  def apply[A](data: Set[Point[A]], dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    if (dim != 2) throw new Exception("FractionnalLastTree can only be constructed with d=2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortBy(p => (p.coord(d), p.id)))
    this.buildFCLastTree(sortedPoints, 0, dim)
  }

  def buildFCLastTree[A](sortedPoints: List[Array[Point[A]]], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    val initialArrayNode = ArrayPoints(sortedPoints.last)
    this.buildFCLastTree(sortedPoints, initialArrayNode, depth, dim)
  }

  def buildFCLastTree[A](sortedPoints: List[Array[Point[A]]], arrayNode: ArrayPoints[A], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    if (sortedPoints(0).length == 1) {
      FractionnalLastTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => d.<=(pivot, depth)))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !(d.<=(pivot, depth))))

      val (leftANode, rightANode) = arrayNode.split(pivot, leftSortedPoints(0).length)
      FractionnalLastTreeNode(pivot, arrayNode, FractionnalLastTree.buildFCLastTree(leftSortedPoints, leftANode, depth, dim), FractionnalLastTree.buildFCLastTree(rightSortedPoints, rightANode, depth, dim), depth)
    }
  }
}

abstract class FractionnalLastTree[A](val value: Point[A],
  val depth: Int) extends FractionalCascading[A] {

  def query(region: SpaceRegion[A]): Set[Point[A]] = {
    val splitNode = this.findSplitNode(region)
    splitNode.queryFromSplitNode(region)
  }

  def findSplitNode(region: SpaceRegion[A]): FractionnalLastTree[A]

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]]
  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]]
  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]]

  def reportSubtree(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]]
}

case class FractionnalLastTreeNode[A](valueNode: Point[A],
  val array: ArrayPoints[A],
  val left: FractionnalLastTree[A],
  val right: FractionnalLastTree[A],
  depthNode: Int) extends FractionnalLastTree[A](valueNode, depthNode) {

  def findSplitNode(region: SpaceRegion[A]): FractionnalLastTree[A] = {
    if (region.contains(depth, value.coord(depth))) {
      this
    } else {
      if (region.sidecontains(depth, value.coord(depth))) {
        left.findSplitNode(region)
      } else {
        right.findSplitNode(region)
      }
    }
  }

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    val (low, up) = array.binarySearch(region)
    left.queryFromSplitNodeLeft(region, array.deepLeft(low), array.deepLeft(up)) ++ right.queryFromSplitNodeRight(region, array.deepRight(low), array.deepRight(up))
  }

  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    if (region.leftcontains(depth, value.coord(depth))) {
      right.reportSubtree(region, array.deepRight(low), array.deepRight(up)) ++
        left.queryFromSplitNodeLeft(region, array.deepLeft(low), array.deepLeft(up))
    } else {
      right.queryFromSplitNodeLeft(region, array.deepRight(low), array.deepRight(up))
    }
  }

  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    if (region.rightcontains(depth, value.coord(depth))) {
      left.reportSubtree(region, array.deepLeft(low), array.deepLeft(up)) ++
        right.queryFromSplitNodeRight(region, array.deepRight(low), array.deepRight(up))
    } else {
      left.queryFromSplitNodeRight(region, array.deepLeft(low), array.deepLeft(up))
    }
  }

  def reportSubtree(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    array.reportSlice(low, up) ++ array.reportSinglePoint(up, region)
  }

  def getLeftTree(): FractionnalLastTree[A] = left

  def getRightTree(): FractionnalLastTree[A] = right

  def getAssoTree(): FractionnalLastTree[A] = null

  def getValue(): Point[A] = value

  def getDepth(): Int = depth
}

case class FractionnalLastTreeLeaf[A](valueLeaf: Point[A],
  depthLeaf: Int) extends FractionnalLastTree[A](valueLeaf, depthLeaf) {

  def findSplitNode(region: SpaceRegion[A]): FractionnalLastTree[A] = {
    this
  }

  def queryFromSplitNode(region: SpaceRegion[A]): Set[Point[A]] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    reportIfIn(region)
  }
  def reportSubtree(region: SpaceRegion[A], low: Int, up: Int): Set[Point[A]] = {
    reportIfIn(region)
  }

  def reportIfIn(region: SpaceRegion[A]): Set[Point[A]] = {
    if (region.contains(valueLeaf)) {
      Set(valueLeaf)
    } else {
      Set()
    }
  }

  def getLeftTree(): FractionnalLastTree[A] = null

  def getRightTree(): FractionnalLastTree[A] = null

  def getAssoTree(): FractionnalLastTree[A] = null

  def getValue(): Point[A] = value

  def getDepth(): Int = depth
}

object test extends App {
  val points = Array(1, 4, 5, 6, 8, 1, 5, 9, 3, 7).zip(Array(5, 6, 8, 9, 1, 2, 3, 5, 4, 3)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))
  println(points.mkString(","))
  println(points.size)
  val tree = FractionnalLastTree(points.toSet, 2)
  val queryresult = tree.query(SpaceRegion(Array(Some(0), Some(0)), Array(Some(10), Some(10))))
  println(queryresult)
  println(queryresult.size)
}
