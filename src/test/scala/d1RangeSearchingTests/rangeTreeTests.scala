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

  "The search " should " not contain results out of the bounds." in {
    boundTest(40, 10, 70, 60)
    boundTest(10, 58, 23, 69)
    boundTest(0, 0, 10, 50)
  }

  // Testing on Strings and 3D points
  val points3DStrings = Array("tree", "coffee", "door", "window", "linux", "penguin", "table", "chair", "strong", "bucket")
    .zip(Array("j", "km", "ml", "jlj", "okm", "jkl", "pm", "pjklj", "pmkfcf", "ppp"))
    .zip(Array("apple", "peach", "banana", "strawberry", "pear", "pineapple", "coconut", "raspberry", "cherry", "kiwi"))
    .map(x => Array(x._1._1, x._1._2, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  // Builds the tree of dimension 3 corresponding to the points
  val treeString = RangeTree(points3DStrings.toSet, 3)

  "The search (3D on Strings) " should " contain all the results in the bounds." in {
    assert(treeString.rangeQuery(SpaceRegion(Array(Some("a"), Some("a"), Some("a")), Array(Some("p"), Some("p"), Some("p")))).size == 1)
    assert(treeString.rangeQuery(SpaceRegion(Array(Some("a"), Some("a"), Some("a")), Array(Some("q"), Some("q"), Some("q")))).size == 5)
  }

  "The search (3D on Strings) " should " not contain results out of the bounds." in {
    boundTest3DString("a", "a", "a", "z", "z", "z")
    boundTest3DString("c", "c", "e", "q", "t", "e")
  }

  def boundTest(lbx: Int, lby: Int, ubx: Int, uby: Int) = {
    var searchSet = tree.rangeQuery(SpaceRegion(Array(Some(lbx), Some(lby)), Array(Some(ubx), Some(uby))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).coord(0) <= ubx && searchPoints(i).coord(1) <= uby)
      assert(searchPoints(i).coord(0) >= lbx && searchPoints(i).coord(1) >= lby)
    }
  }

  def boundTest3DString(lbx: String, lby: String, lbz: String, ubx: String, uby: String, ubz: String) = {
    var searchSet = treeString.rangeQuery(SpaceRegion(Array(Some(lbx), Some(lby), Some(lbz)), Array(Some(ubx), Some(uby), Some(ubz))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).coord(0) <= ubx && searchPoints(i).coord(1) <= uby)
      assert(searchPoints(i).coord(0) >= lbx && searchPoints(i).coord(1) >= lby)
    }
  }

}