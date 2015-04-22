package d1RangeSearchingTests
import org.scalatest._
import kdTrees._

class kdTreesTests extends FlatSpec {

  // Points are : [3;5], [10;20], [19;70], [25;93], [33 :17], [41;63], [55,14], [62;59], [83;47], [91;73]
  val points = Array(3, 10, 19, 25, 33, 41, 55, 62, 83, 91).zip(Array(5, 20, 70, 93, 17, 63, 14, 59, 47, 73)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  "The points " should " all be correctly created." in {
    assert(points(0).coord(0) == 3)
    assert(points(0).coord(1) == 5)
    assert(points(1).coord(0) == 10)
    assert(points(1).coord(1) == 20)
    assert(points(2).coord(0) == 19)
    assert(points(2).coord(1) == 70)
    assert(points(3).coord(0) == 25)
    assert(points(3).coord(1) == 93)
    assert(points.size == 10)
  }

  // Builds the tree of dimension 2 corresponding to the points
  val tree = KdTree(points.toSet, 2)
  "The kdTree " should " be correctly build." in {
    assert(tree.value.coord(0) == 33)
    assert(tree.value.coord(1) == 17)
    val rightTree = tree.getRightTree()
    val leftTree = tree.getLeftTree()
    assert(leftTree.value.coord(0) == 10)
    assert(leftTree.value.coord(1) == 20)
    assert(rightTree.value.coord(0) == 62)
    assert(rightTree.value.coord(1) == 59)

  }

  // Tests on the search results
  "The search " should " return (only) the points in the search space." in {
    var searchSet = tree.searchKD(SpaceRegion(Array(Some(40), Some(10)), Array(Some(70), Some(60))))

    assert(searchSet.size == 2)
    //    assert(searchSet.toString().equals("Set((55,14), (62,59))"))
    assert(searchSet.contains(points(6)))
    assert(searchSet.contains(points(7)))
    assert(!searchSet.contains(points(0)))

    searchSet = tree.searchKD(SpaceRegion(Array(Some(0), Some(0)), Array(Some(1000), Some(1000))))
    assert(searchSet.size == 10)

    searchSet = tree.searchKD(SpaceRegion(Array(Some(0), Some(0)), Array(Some(1), Some(1))))
    assert(searchSet.size == 0)

    searchSet = tree.searchKD(SpaceRegion(Array(Some(1000), Some(500)), Array(Some(1001), Some(5001))))
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
  val treeString = KdTree(points3DStrings.toSet, 3)

  "The search (3D on Strings) " should " not contain results out of the bounds." in {
    assert(treeString.searchKD(SpaceRegion(Array(Some("a"), Some("a"), Some("a")), Array(Some("p"), Some("p"), Some("p")))).size == 1)
    assert(treeString.searchKD(SpaceRegion(Array(Some("a"), Some("a"), Some("a")), Array(Some("q"), Some("q"), Some("q")))).size == 5)

    boundTest3DString("a", "a", "a", "z", "z", "z")
    boundTest3DString("c", "c", "e", "q", "t", "e")
  }

  def boundTest(lbx: Int, lby: Int, ubx: Int, uby: Int) = {
    var searchSet = tree.searchKD(SpaceRegion(Array(Some(lbx), Some(lby)), Array(Some(ubx), Some(uby))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).coord(0) <= ubx && searchPoints(i).coord(1) <= uby)
      assert(searchPoints(i).coord(0) >= lbx && searchPoints(i).coord(1) >= lby)
    }
  }

  def boundTest3DString(lbx: String, lby: String, lbz: String, ubx: String, uby: String, ubz: String) = {
    var searchSet = treeString.searchKD(SpaceRegion(Array(Some(lbx), Some(lby), Some(lbz)), Array(Some(ubx), Some(uby), Some(ubz))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).coord(0) <= ubx && searchPoints(i).coord(1) <= uby)
      assert(searchPoints(i).coord(0) >= lbx && searchPoints(i).coord(1) >= lby)
    }
  }

}