package d1RangeSearchingTests
import org.scalatest._

class D1TreeTests extends FlatSpec {
  // Values for integer testing
  val bigList = List(3, 10, 19, 23, 30, 37, 49, 59, 62, 70, 80, 89, 100, 105)
  val bigTree = new d1RangeSearching.D1Root().makeNode(bigList)
  val oneNodeTree = new d1RangeSearching.D1Root().makeNode(List(1))

  var subTree1 = bigTree.getLeftTree()
  var subTree11 = subTree1.getLeftTree()
  var subTree111 = subTree11.getLeftTree()
  var subTree1111 = subTree111.getLeftTree()

  var belowSubTree111 = subTree111.reportSubtree

  var request = bigTree.d1RangeQuery(15, 65)

  // Values for String testing
  val stringList = List("b", "z", "a", "d", "k")
  val stringTree = new d1RangeSearching.D1Root().makeNode(stringList)
  val stringRequest = stringTree.d1RangeQuery("c", "l")

  // Integer testing
  "The root node" should "be the median" in {
    assert(bigTree.value == 49)
  }

  "The values of the node" should "be increasing from left to right" in {
    assert(subTree1.reportSubtree.size == 7)
    assert(subTree1.value == 23)
    assert(subTree11.value == 10)
    assert(subTree111.value == 3)
    assert(belowSubTree111.size == 2)
    assert(belowSubTree111.contains(3))
    assert(belowSubTree111.contains(10))
    assert(subTree1111.value == 3)
  }

  "A leaf" should "not have subtrees" in {
    assert(subTree1111.getLeftTree() == null)
    assert(subTree1111.getRightTree() == null)
  }

  "The requests" should "be correctly done" in {
    assert(request.size == 7)
    assert(request.contains(19))
    assert(request.contains(23))
    assert(request.contains(30))
    assert(request.contains(37))
    assert(request.contains(49))
    assert(request.contains(59))
    assert(request.contains(62))
  }

  "Requests out of the range" should "give zero result" in {
    request = bigTree.d1RangeQuery(110, 120)
    assert(request.size == 0)

    request = bigTree.d1RangeQuery(-5, 0)
    assert(request.size == 0)
  }

  "Requests containing the full range" should "return all the nodes" in {
    request = bigTree.d1RangeQuery(-50, 1000)
    assert(request.size == 14)
  }

  "The split node" should "be correct" in {
    assert(bigTree.findSplitNode(5, 22).value == 10)
    assert(bigTree.findSplitNode(40, 60).value == 49)
    assert(bigTree.findSplitNode(0, 2).value == 3)
    assert(bigTree.findSplitNode(150, 188).value == 105)
  }

  "A tree with onlu one node" should "be only the root" in {
    assert(oneNodeTree.getLeftTree() == null)
    assert(oneNodeTree.getRightTree() == null)
  }

  // String testing
  "A tree of Strings" should "be correctly build" in {
    assert(stringTree.reportSubtree.size == 5)
    assert(stringTree.getLeftTree().reportSubtree.size == 3)
    assert(stringTree.getRightTree().reportSubtree.size == 2)
  }
  "A request on Strings" should "give the right results" in {
    assert(stringRequest.size == 2)
    assert(stringRequest.contains("d"))
    assert(stringRequest.contains("k"))
  }
}