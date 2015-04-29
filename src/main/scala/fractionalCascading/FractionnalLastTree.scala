package fractionalCascading

import kdTrees.Point
import kdTrees.SpaceRegion
import scala.collection.mutable.Stack

case class FCNode[A](val value: Point[A],
                     var next: Option[FCNode[A]] = None,
                     var left: Option[FCNode[A]] = None,
                     var right: Option[FCNode[A]] = None) {

  def reportUntil(end: Option[FCNode[A]]): Set[Point[A]] = {
    if (!end.isEmpty && (this == end.get)) {
      Set()
    } else {
      if (next.isEmpty)
        Set(value)
      else
        next.get.reportUntil(end) + value
    }
  }

  def setNext(next: FCNode[A]) = {
    this.next = Some(next)
  }

  def report(region: SpaceRegion[A]) = {
    if (region.contains(value)) {
      Set(value)
    } else {
      Set()
    }
  }

  def length = {
    var current = this.next
    var count = 1
    while (!current.isEmpty) {
      current = current.get.next
      count += 1
    }
    count
  }

  def deepLeft = left.get
  def deepOLeft = left
  def deepRight = right.get
  def deepORight = right

  override def toString: String = {
    if (next.isEmpty) {
      value.toString
    } else {
      value.toString + " -> " + next.get.toString
    }
  }
}

object ArrayNode {
  def apply[A](array: Array[Point[A]]): Array[FCNode[A]] = {
    val newarray = array.map(point => FCNode(point))
    for (i <- 0 until newarray.length - 1) {
      newarray(i).setNext(newarray(i + 1))
    }
    newarray
  }
}
object Split {
  def apply[A](array: Array[FCNode[A]], pivot: Point[A]): (Array[FCNode[A]], Array[FCNode[A]]) = {
    val stack: Stack[FCNode[A]] = Stack(array(0))
    var current = array(0).next
    while (!current.isEmpty) {
      stack.push(current.get)
      current = current.get.next
    }
    var leftS: Option[FCNode[A]] = None
    var leftcount = 0
    var rightS: Option[FCNode[A]] = None
    var rightcount = 0

    while (!stack.isEmpty) {
      val elem = stack.pop
      if (elem.value.<=(pivot, pivot.dim - 2)) {
        leftS = Some(FCNode(elem.value, leftS))
        leftcount += 1
      } else {
        rightS = Some(FCNode(elem.value, rightS))
        rightcount += 1
      }
      elem.left = leftS
      elem.right = rightS
    }
    current = leftS
    val leftArray = Array.tabulate(leftcount) { i =>
      val temp = current.get
      current = temp.next
      temp
    }
    current = rightS
    val rightArray = Array.tabulate(rightcount) { i =>
      val temp = current.get
      current = temp.next
      temp
    }
    (leftArray, rightArray)
  }
}
object BinarySearch {
  def apply[A](array: Array[FCNode[A]], region: SpaceRegion[A]): (FCNode[A], FCNode[A]) = {
    val dim = array(0).value.dim
    val lb = region.lb(dim - 1)
    val ub = region.ub(dim - 1)
    val first = if (lb.isEmpty) {
      array(0)
    } else {
      BinarySearch(array, lb.get, 0, array.length - 1)
    }
    val last = if (ub.isEmpty) {
      array.last
    } else {
      BinarySearch(array, ub.get, 0, array.length - 1)
    }
    (first, last)
  }
  def apply[A](array: Array[FCNode[A]], bound: A, from: Int, to: Int): FCNode[A] = {
    if (from == to) {
      array(from)
    } else {
      val dim = array(0).value.dim
      val pivot = (from + to) / 2
      if (array(pivot).value.>=(bound, dim - 1)) {
        BinarySearch(array, bound, from, pivot)
      } else {
        BinarySearch(array, bound, pivot + 1, to)
      }
    }
  }
}

object FractionnalLastTree {
  def apply[A](data: Set[Point[A]], dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    if (dim != 2) throw new Exception("FractionnalLastTree can only be constructed with d=2")
    val sortedPoints = List.tabulate(dim)(d => data.toArray.sortBy(p => (p.coord(d), p.id)))
    this.buildFCLastTree(sortedPoints, 0, dim)
  }

  def buildFCLastTree[A](sortedPoints: List[Array[Point[A]]], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    val initialArrayNode = ArrayNode(sortedPoints.last)
    this.buildFCLastTree(sortedPoints, initialArrayNode, 0, dim)
  }

  def buildFCLastTree[A](sortedPoints: List[Array[Point[A]]], arrayNode: Array[FCNode[A]], depth: Int, dim: Int)(implicit ord: Ordering[A]): FractionnalLastTree[A] = {
    if (sortedPoints(0).length == 1) {
      arrayNode.foreach { node =>
        val newnode = FCNode(node.value)
        node.left = Some(newnode)
        node.right = Some(newnode)
      }
      FractionnalLastTreeLeaf(sortedPoints(0)(0), depth)
    } else {
      val sublength = (sortedPoints(0).length - 1) / 2
      val pivot = sortedPoints(0)(sublength)
      val pivotCode = (pivot.coord(depth), pivot.id)

      val leftSortedPoints = sortedPoints.map(data => data.filter(d => ord.lt(d.coord(depth), pivotCode._1) || ((ord.equiv(d.coord(depth), pivotCode._1) && d.id <= pivotCode._2))))
      val rightSortedPoints = sortedPoints.map(data => data.filter(d => !((ord.lt(d.coord(depth), pivotCode._1) || ((ord.equiv(d.coord(depth), pivotCode._1) && d.id <= pivotCode._2))))))

      val (leftANode, rightANode) = Split(arrayNode, pivot)
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
  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]]
  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]]

  def reportSubtree(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]]
}

case class FractionnalLastTreeNode[A](valueNode: Point[A],
                                      val array: Array[FCNode[A]],
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
    val (low, up) = BinarySearch(array, region)
    left.queryFromSplitNodeLeft(region, low.deepOLeft, up.deepOLeft) ++ right.queryFromSplitNodeRight(region, low.deepORight, up.deepORight)
  }

  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
    if (region.leftcontains(depth, value.coord(depth))) {
      right.reportSubtree(region, low.flatMap(_.deepORight), up.flatMap(_.deepORight)) ++
        left.queryFromSplitNodeLeft(region, low.flatMap(_.deepOLeft), up.flatMap(_.deepOLeft))
    } else {
      right.queryFromSplitNodeLeft(region, low.flatMap(_.deepORight), up.flatMap(_.deepORight))
    }
  }

  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
    if (region.rightcontains(depth, value.coord(depth))) {
      left.reportSubtree(region, low.flatMap(_.deepOLeft), up.flatMap(_.deepOLeft)) ++
        right.queryFromSplitNodeRight(region, low.flatMap(_.deepORight), up.flatMap(_.deepORight))
    } else {
      left.queryFromSplitNodeRight(region, low.flatMap(_.deepOLeft), up.flatMap(_.deepOLeft))
    }
  }

  def reportSubtree(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
    if (low.isEmpty)
      Set()
    else
      low.get.reportUntil(up) ++ (if (up.isEmpty) Set() else up.get.report(region))
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
  def queryFromSplitNodeLeft(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
    reportIfIn(region)
  }
  def queryFromSplitNodeRight(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
    reportIfIn(region)
  }
  def reportSubtree(region: SpaceRegion[A], low: Option[FCNode[A]], up: Option[FCNode[A]]): Set[Point[A]] = {
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
