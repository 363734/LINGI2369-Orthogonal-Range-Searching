package d1RangeSearchingTests
import org.scalatest._
import rangeTree._
import kdTrees._

class rangeTreeTests extends FlatSpec {
  // Points are : [3;5], [10;20], [19;70], [25;93], [33 :17], [41;63], [55,14], [62;59], [83;47], [91;73]
  val points = Array(3, 10, 19, 25, 33, 41, 55, 62, 83, 91).zip(Array(5, 20, 70, 93, 17, 63, 14, 59, 47, 73)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  // Builds the tree of dimension 2 corresponding to the points
  val tree = RangeTree(points.toSet, 2)

  val rightTree = tree.getRightTree()
  val leftTree = tree.getLeftTree()
  val rlTree = rightTree.getLeftTree()
  "The first dimention tree " should " be correctly build." in {
    assert(tree.value.coord(0) == 33)
    assert(tree.value.coord(1) == 17)
    assert(leftTree.value.coord(0) == 19)
    assert(leftTree.value.coord(1) == 70)
    assert(rightTree.value.coord(0) == 62)
    assert(rightTree.value.coord(1) == 59)
    assert(rlTree.value.coord(0) == 55)
    assert(rlTree.value.coord(1) == 14)
  }

  "The depth of the nodes " should "be correct" in {
    assert(tree.depth == 0)
    assert(leftTree.depth == 0)
    assert(rightTree.depth == 0)
    assert(rlTree.depth == 0)
  }

  val rlAssoTree = rlTree.getAssoTree().get

  "The second dimention tree " should " be correctly build." in {
    assert(rlAssoTree.depth == 1)
    assert(rlAssoTree.getAssoTree() == None)
    assert(rlAssoTree.value.coord(0) == 62)
    assert(rlAssoTree.value.coord(1) == 59)

    val rightTree = rlAssoTree.getRightTree()
    assert(rightTree.getLeftTree() == null)
    assert(rightTree.getRightTree() == null)
    assert(rightTree.getAssoTree() == None)
    assert(rightTree.value.coord(0) == 41)
    assert(rightTree.value.coord(1) == 63)

    val leftTree = rlAssoTree.getLeftTree()
    assert(leftTree.getLeftTree() != null)
    assert(leftTree.getRightTree() != null)
    assert(leftTree.getAssoTree() == None)
    assert(leftTree.value.coord(0) == 55)
    assert(leftTree.value.coord(1) == 14)
  }

  "The search " should " return correct results." in {
    var searchSet = tree.rangeQuery(SpaceRegion(Array(Some(40), Some(10)), Array(Some(70), Some(60))))

    assert(searchSet.size == 2)
    assert(searchSet.contains(points(6)))
    assert(searchSet.contains(points(7)))
    assert(!searchSet.contains(points(0)))
    //    println(searchSet.toString())
    //    assert(searchSet.toString().equals("Set((55,14), (62,59))")
    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(55), Some(14)), Array(Some(62), Some(59))))
    assert(searchSet.size == 2)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(56), Some(14)), Array(Some(62), Some(59))))
    assert(searchSet.size == 1)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(55), Some(15)), Array(Some(62), Some(59))))
    assert(searchSet.size == 1)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(55), Some(14)), Array(Some(61), Some(59))))
    assert(searchSet.size == 1)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(55), Some(14)), Array(Some(62), Some(58))))
    assert(searchSet.size == 1)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(0), Some(0)), Array(Some(1000), Some(1000))))
    assert(searchSet.size == 10)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(0), Some(0)), Array(Some(1), Some(1))))
    assert(searchSet.size == 0)

    searchSet = tree.rangeQuery(SpaceRegion(Array(Some(1000), Some(500)), Array(Some(1001), Some(5001))))
    assert(searchSet.size == 0)
  }

}