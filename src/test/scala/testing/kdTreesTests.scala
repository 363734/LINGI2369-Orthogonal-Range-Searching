package testing
import org.scalatest._
import kdTrees._
import space._

class kdTreesTests extends FlatSpec {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  implicit val boundString = ("", "zzzzz")

  // Points are : [3;5], [10;20], [19;70], [25;93], [33 :17], [41;63], [55,14], [62;59], [83;47], [91;73]
  val points = Array(3, 10, 19, 25, 33, 41, 55, 62, 83, 91).zip(Array(5, 20, 70, 93, 17, 63, 14, 59, 47, 73)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  "The points " should " all be correctly created." in {
    assert(points(0).coord(1) == 3)
    assert(points(0).coord(2) == 5)
    assert(points(1).coord(1) == 10)
    assert(points(1).coord(2) == 20)
    assert(points(2).coord(1) == 19)
    assert(points(2).coord(2) == 70)
    assert(points(3).coord(1) == 25)
    assert(points(3).coord(2) == 93)
    assert(points.size == 10)
  }

  // Builds the tree of dimension 2 corresponding to the points
  val tree = KdTree(points.toSet, 2)
  // Get the subtrees
  val rightTree = tree.getRightTree()
  val leftTree = tree.getLeftTree()

  "The kdTree " should " be correctly build." in {
    assert(tree.value.asInstanceOf[Point[Int]].coord(1) == 33)
    assert(tree.value.asInstanceOf[Point[Int]].coord(2) == 17)

    assert(leftTree.value.asInstanceOf[Point[Int]].coord(1) == 10)
    assert(leftTree.value.asInstanceOf[Point[Int]].coord(2) == 20)
    assert(rightTree.value.asInstanceOf[Point[Int]].coord(1) == 62)
    assert(rightTree.value.asInstanceOf[Point[Int]].coord(2) == 59)
  }

  // Tests on the search results
  "The search " should " return (only) the points in the search space" in {
    var searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(40, 10)), Point(-1, Array(70, 60))))

    assert(searchSet.size == 2)

    assert(searchSet.contains(points(6)))
    assert(searchSet.contains(points(7)))
    assert(!searchSet.contains(points(0)))

    // Reduces the search space to exclusively those points
    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 2)

    // Reduces the search space to exclude on of those points on one dimension
    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(56, 14)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(55, 15)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(61, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(62, 58))))
    assert(searchSet.size == 1)
  }

  "A search space containing all the points " should " report all the points" in {
    val searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(0, 0)), Point(-1, Array(1000, 1000))))
    assert(searchSet.size == 10)
  }

  "A search space out of the bounds of the points " should " report no point" in {
    var searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(0, 0)), Point(-1, Array(1, 1))))
    assert(searchSet.size == 0)

    searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(1000, 500)), Point(-1, Array(1001, 5001))))
    assert(searchSet.size == 0)
  }

  "The search " should " not contain results out of the bounds." in {
    boundTest(40, 10, 70, 60)
    boundTest(10, 58, 23, 69)
    boundTest(0, 0, 10, 50)
  }

  /**
   * Function testing that the search results are in the bounds of the search space for 2D Int points
   */
  def boundTest(lbx: Int, lby: Int, ubx: Int, uby: Int) = {
    var searchSet = tree.searchKD(SpaceRegion(Point(-1, Array(lbx, lby)), Point(-1, Array(ubx, uby))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).asInstanceOf[Point[Int]].coord(1) <= ubx && searchPoints(i).asInstanceOf[Point[Int]].coord(2) <= uby)
      assert(searchPoints(i).asInstanceOf[Point[Int]].coord(1) >= lbx && searchPoints(i).asInstanceOf[Point[Int]].coord(2) >= lby)
    }
  }

  // Testing on Strings and 3D points
  val points3DStrings = Array("tree", "coffee", "door", "window", "linux", "penguin", "table", "chair", "strong", "bucket")
    .zip(Array("j", "km", "ml", "jlj", "okm", "jkl", "pm", "pjklj", "pmkfcf", "ppp"))
    .zip(Array("apple", "peach", "banana", "strawberry", "pear", "pineapple", "coconut", "raspberry", "cherry", "kiwi"))
    .map(x => Array(x._1._1, x._1._2, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  // Builds the tree of dimension 3 corresponding to the points
  val treeString = KdTree(points3DStrings.toSet, 3)

  "The search (3D on Strings) " should " contain all the results in the bounds." in {
    assert(treeString.searchKD(SpaceRegion(Point(-1, Array("a", "a", "a")), Point(-1, Array("p", "p", "p")))).size == 1)
    assert(treeString.searchKD(SpaceRegion(Point(-1, Array("a", "a", "a")), Point(-1, Array("q", "q", "q")))).size == 5)
  }

  "The search (3D on Strings) " should " not contain results out of the bounds." in {
    boundTest3DString("a", "a", "a", "z", "z", "z")
    boundTest3DString("c", "c", "e", "q", "t", "e")
  }

  /**
   * Function testing that the search results are in the bounds of the search space for 3D String points
   */
  def boundTest3DString(lbx: String, lby: String, lbz: String, ubx: String, uby: String, ubz: String) = {
    var searchSet = treeString.searchKD(SpaceRegion(Point(-1, Array(lbx, lby, lbz)), Point(-1, Array(ubx, uby, ubz))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).asInstanceOf[Point[String]].coord(1) <= ubx
        && searchPoints(i).asInstanceOf[Point[String]].coord(2) <= uby
        && searchPoints(i).asInstanceOf[Point[String]].coord(3) <= ubz)
      assert(searchPoints(i).asInstanceOf[Point[String]].coord(1) >= lbx
        && searchPoints(i).asInstanceOf[Point[String]].coord(2) >= lby
        && searchPoints(i).asInstanceOf[Point[String]].coord(3) >= lbz)
    }
  }

}