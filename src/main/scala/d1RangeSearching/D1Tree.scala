package d1RangeSearching

abstract class D1Tree[A](val value: A)(implicit ord: Ordering[A]) {

  def findSplitNode(lb: A, ub: A): D1Tree[A]

  def reportSubtree: Set[A]

  def d1RangeQuery(lb: A, ub: A): Set[A] = {
    val splitNode = this.findSplitNode(lb, ub)
    splitNode.d1RangeQueryFromSplitNode(lb, ub)
  }

  def d1RangeQueryFromSplitNode(lb: A, ub: A): Set[A]
  def d1RangeQueryFromSplitNodeLeft(lb: A): Set[A]
  def d1RangeQueryFromSplitNodeRight(ub: A): Set[A]
  def getRightTree(): D1Tree[A]
  def getLeftTree(): D1Tree[A]

  override def toString: String = value.toString
}

case class D1Node[A](valueNode: A, val left: D1Tree[A], val right: D1Tree[A])(implicit ord: Ordering[A]) extends D1Tree[A](valueNode) {

  def findSplitNode(lb: A, ub: A): D1Tree[A] = {
    if (ord.lteq(lb, value) && ord.lteq(value, ub)) { // lb <= value && value <= ub
      this
    } else {
      if (ord.lteq(ub, value)) // ub <= v
        left.findSplitNode(lb, ub)
      else
        right.findSplitNode(lb, ub)
    }
  }

  def reportSubtree: Set[A] = {
    left.reportSubtree ++ right.reportSubtree
  }

  def d1RangeQueryFromSplitNode(lb: A, ub: A): Set[A] = {
    left.d1RangeQueryFromSplitNodeLeft(lb) ++ right.d1RangeQueryFromSplitNodeRight(ub)
  }

  def d1RangeQueryFromSplitNodeLeft(lb: A): Set[A] = {
    if (ord.lteq(lb, value)) // lb <= value
      right.reportSubtree ++ left.d1RangeQueryFromSplitNodeLeft(lb)
    else
      right.d1RangeQueryFromSplitNodeLeft(lb)
  }

  def d1RangeQueryFromSplitNodeRight(ub: A): Set[A] = {
    if (ord.lteq(value, ub)) // value <= ub
      left.reportSubtree ++ right.d1RangeQueryFromSplitNodeRight(ub)
    else
      left.d1RangeQueryFromSplitNodeRight(ub)
  }

  def getLeftTree(): D1Tree[A] = left

  def getRightTree(): D1Tree[A] = right

  override def toString: String = "node : " + value.toString

}

case class D1Leaf[A](valueLeaf: A)(implicit ord: Ordering[A]) extends D1Tree[A](valueLeaf) {

  def findSplitNode(lb: A, ub: A): D1Tree[A] = this

  def reportSubtree: Set[A] = Set(value)

  def d1RangeQueryFromSplitNode(lb: A, ub: A): Set[A] = {
    if (ord.lteq(lb, value) && ord.lteq(value, ub)) // lb <= value && value <= ub
      Set(value)
    else
      Set()
  }

  def d1RangeQueryFromSplitNodeLeft(lb: A): Set[A] = {
    if (ord.lteq(lb, value)) // lb <= value
      Set(value)
    else
      Set()
  }

  def d1RangeQueryFromSplitNodeRight(ub: A): Set[A] = {
    if (ord.lteq(value, ub)) // value <= ub
      Set(value)
    else
      Set()
  }

  def getLeftTree(): D1Tree[A] = null

  def getRightTree(): D1Tree[A] = null

  def getValue(): A = valueLeaf

  override def toString: String = "leaf : " + value.toString

}

case class D1Root[A]() {

  def makeNode(valuesList: List[A])(implicit ord: Ordering[A]): D1Tree[A] = {

    val ordererList = valuesList.sortWith(ord.lteq(_, _))
    val listSize = valuesList.size

    if (listSize > 1) {
      val mid = listSize / 2 + listSize % 2
      val separation = ordererList.grouped(mid).toList
      val leftList = separation(0)
      val rightList = separation(1)
      D1Node(ordererList(mid - 1), makeNode(leftList), makeNode(rightList))
    } else if (listSize == 1) {
      D1Leaf(ordererList(0))
    } else {
      D1Leaf(ordererList(1))
    }

  }
}

object test extends App {

  //  val a1 = List(23,10,3,19)
  // Tests on a "big" list of integer
  val bigList = List(3, 10, 19, 23, 30, 37, 49, 59, 62, 70, 80, 89, 100, 105)

  val bigTree = new D1Root().makeNode(bigList)
  assert(bigTree.value == 49)

  var subTree1 = bigTree.getLeftTree()
  assert(subTree1.reportSubtree.size == 7)
  assert(subTree1.value == 23)
  var subTree11 = subTree1.getLeftTree()
  assert(subTree11.value == 10)
  var subTree111 = subTree11.getLeftTree()
  assert(subTree111.value == 3)
  var belowSubTree111 = subTree111.reportSubtree
  assert(belowSubTree111.size == 2)
  assert(belowSubTree111.contains(3))
  assert(belowSubTree111.contains(10))
  var subTree1111 = subTree111.getLeftTree()
  assert(subTree1111.value == 3)
  assert(subTree1111.getLeftTree() == null)
  assert(subTree1111.getRightTree() == null)

  var subTree2 = bigTree.getRightTree()
  assert(subTree2.reportSubtree.size == 7)
  assert(subTree2.value == 80)

  var request = bigTree.d1RangeQuery(15, 65)

  assert(request.size == 7)
  assert(request.contains(19))
  assert(request.contains(23))
  assert(request.contains(30))
  assert(request.contains(37))
  assert(request.contains(49))
  assert(request.contains(59))
  assert(request.contains(62))

  request = bigTree.d1RangeQuery(110, 120)
  assert(request.size == 0)

  request = bigTree.d1RangeQuery(-5, 0)
  assert(request.size == 0)

  request = bigTree.d1RangeQuery(-50, 1000)
  assert(request.size == 14)

  assert(bigTree.findSplitNode(5, 22).value == 10)
  assert(bigTree.findSplitNode(40, 60).value == 49)
  assert(bigTree.findSplitNode(0, 2).value == 3)
  assert(bigTree.findSplitNode(150, 188).value == 105)

  // Tests on a list of Strings
  val stringList = List("b", "z", "a", "d", "k")
  val stringTree = new D1Root().makeNode(stringList)

  assert(stringTree.reportSubtree.size == 5)
  assert(stringTree.getLeftTree().reportSubtree.size == 3)
  assert(stringTree.getRightTree().reportSubtree.size == 2)

  val stringRequest = stringTree.d1RangeQuery("c", "l")
  assert(stringRequest.size == 2)
  assert(stringRequest.contains("d"))
  assert(stringRequest.contains("k"))

  val oneNodeTree = new D1Root().makeNode(List(1))
  assert(oneNodeTree.getLeftTree() == null)
  assert(oneNodeTree.getRightTree() == null)

  //  val tree1 = D1Node(3, D1Leaf(3), D1Leaf(10))
  //  val tree2 = D1Node(19, D1Leaf(19), D1Leaf(23))
  //  val tree = D1Node(10, tree1, tree2)

  //  println(tree1.d1RangeQueryFromSplitNodeRight(11))
  //  println(tree1.reportSubTree)
  //  println(tree2.reportSubTree)
  //  println(bigTree.findSplitNode(5, 22))
  //  println(bigTree.d1RangeQueryFromSplitNodeLeft(5))
  //  println(bigTree.d1RangeQueryFromSplitNodeRight(22))

  //  println(bigTree.d1RangeQuery(5, 22))

}