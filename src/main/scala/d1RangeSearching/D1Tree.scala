package d1RangeSearching

/**
 * Abstract class defining a search tree in one dimension
 */
abstract class D1Tree[A](val value: A)(implicit ord: Ordering[A]) {

  /**
   *  Returns the split node from the search
   */
  def findSplitNode(lb: A, ub: A): D1Tree[A]

  /**
   * Report all the points in the subtree of the node
   */
  def reportSubtree: Set[A]

  /**
   * Performs a query on this node and returns the set of points in the search space
   */
  def d1RangeQuery(lb: A, ub: A): Set[A] = {
    val splitNode = this.findSplitNode(lb, ub)
    splitNode.d1RangeQueryFromSplitNode(lb, ub)
  }

  /**
   * Group of functions the query on this node :
   * d1RangeQueryFromSplitNode : on the current Node
   * d1RangeQueryFromSplitNodeLeft : on a left childNode
   * d1RangeQueryFromSplitNodeRight : on a right childNode
   */
  def d1RangeQueryFromSplitNode(lb: A, ub: A): Set[A]
  def d1RangeQueryFromSplitNodeLeft(lb: A): Set[A]
  def d1RangeQueryFromSplitNodeRight(ub: A): Set[A]

  /**
   * Getter functions for the sub-trees
   */
  def getRightTree(): D1Tree[A]
  def getLeftTree(): D1Tree[A]

  override def toString: String = value.toString
}

/**
 * Class implementing D1Tree reprensenting an internal node of the tree
 */
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

object D1Tree {

  /**
   *  Constructor of the 1D tree
   *  In : A set of values subject to an ordering
   *  Out : A one dimensional search tree
   */
  def apply[A](data: Set[A])(implicit ord: Ordering[A]): D1Tree[A] = {
    // TODO: check all points have dimention dim and unique id
    val sortedPoints = data.toList.sortWith(ord.lteq(_, _))

    this.build1DTree(sortedPoints)
  }

  /**
   * Starts the creation of tree from a list of points
   * The functions orders them and then creates the tree
   */
  def build1DTree[A](valuesList: List[A])(implicit ord: Ordering[A]): D1Tree[A] = {

    val ordererList = valuesList.sortWith(ord.lteq(_, _))
    val listSize = valuesList.size

    if (listSize > 1) {
      val mid = listSize / 2 + listSize % 2
      val separation = ordererList.grouped(mid).toList
      val leftList = separation(0)
      val rightList = separation(1)
      D1Node(ordererList(mid - 1), build1DTree(leftList), build1DTree(rightList))
    } else if (listSize == 1) {
      D1Leaf(ordererList(0))
    } else {
      D1Leaf(ordererList(1))
    }

  }
}

object test extends App {

  val tree1 = D1Node(3, D1Leaf(3), D1Leaf(10))
  val tree2 = D1Node(19, D1Leaf(19), D1Leaf(23))
  val tree = D1Node(10, tree1, tree2)

  println(tree1.d1RangeQueryFromSplitNodeRight(11))
  println(tree1.reportSubtree)
  println(tree2.reportSubtree)

}