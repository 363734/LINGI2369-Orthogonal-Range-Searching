package d1RangeSearchingTests
import org.scalatest._
import space._
import fractionalCascading._

class fractionalCascadingTests extends FlatSpec {
  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  implicit val boundString = ("", "zzzzz")
  // Points are : [3;5], [10;20], [19;70], [25;93], [33 :17], [41;63], [55,14], [62;59], [83;47], [91;73]
  val points = Array(3, 10, 19, 25, 33, 41, 55, 62, 83, 91).zip(Array(5, 20, 70, 93, 17, 63, 14, 59, 47, 73)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  val tree = FractionnalTree(points.toSet, 2)

  val rightTree = tree.getRightTree()
  val leftTree = tree.getLeftTree()
  val rlTree = rightTree.getLeftTree()
  "The first dimention tree " should " be correctly build." in {
    assert(tree.getValue().asInstanceOf[Point[Int]].coord(1) == 33)
    assert(tree.getValue().asInstanceOf[Point[Int]].coord(2) == 17)
    assert(leftTree.getValue().asInstanceOf[Point[Int]].coord(1) == 19)
    assert(leftTree.getValue().asInstanceOf[Point[Int]].coord(2) == 70)
    assert(rightTree.getValue().asInstanceOf[Point[Int]].coord(1) == 62)
    assert(rightTree.getValue().asInstanceOf[Point[Int]].coord(2) == 59)
    assert(rlTree.getValue().asInstanceOf[Point[Int]].coord(1) == 55)
    assert(rlTree.getValue().asInstanceOf[Point[Int]].coord(2) == 14)
  }

  "The depth of the nodes " should "be correct" in {
    assert(tree.getDepth() == 1)
    assert(leftTree.getDepth() == 1)
    assert(rightTree.getDepth() == 1)
    assert(rlTree.getDepth() == 1)
  }

  "The search " should " return correct results." in {
    var searchSet = tree.query(SpaceRegion(Point(-1, Array(40, 10)), Point(-1, Array(70, 60))))

    assert(searchSet.size == 2)
    assert(searchSet.contains(points(6)))
    assert(searchSet.contains(points(7)))
    assert(!searchSet.contains(points(0)))

    searchSet = tree.query(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 2)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(56, 14)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(55, 15)), Point(-1, Array(62, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(61, 59))))
    assert(searchSet.size == 1)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(55, 14)), Point(-1, Array(62, 58))))
    assert(searchSet.size == 1)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(0, 0)), Point(-1, Array(1000, 1000))))
    assert(searchSet.size == 10)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(0, 0)), Point(-1, Array(1, 1))))
    assert(searchSet.size == 0)

    searchSet = tree.query(SpaceRegion(Point(-1, Array(1000, 500)), Point(-1, Array(1001, 5001))))
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
  val treeString = FractionnalTree(points3DStrings.toSet, 3)

  "The search (3D on Strings) " should " contain all the results in the bounds." in {
    assert(treeString.query(SpaceRegion(Point(-1, Array("a", "a", "a")), Point(-1, Array("p", "p", "p")))).size == 1)
    assert(treeString.query(SpaceRegion(Point(-1, Array("a", "a", "a")), Point(-1, Array("q", "q", "q")))).size == 5)
  }

  "The search (3D on Strings) " should " not contain results out of the bounds." in {
    boundTest3DString("a", "a", "a", "z", "z", "z")
    boundTest3DString("c", "c", "e", "q", "t", "e")
  }

  def boundTest(lbx: Int, lby: Int, ubx: Int, uby: Int) = {
    var searchSet = tree.query(SpaceRegion(Point(-1, Array(lbx, lby)), Point(-1, Array(ubx, uby))))

    var searchPoints = searchSet.toList
    for (i <- 0 until searchPoints.size) {
      assert(searchPoints(i).asInstanceOf[Point[Int]].coord(1) <= ubx && searchPoints(i).asInstanceOf[Point[Int]].coord(2) <= uby)
      assert(searchPoints(i).asInstanceOf[Point[Int]].coord(1) >= lbx && searchPoints(i).asInstanceOf[Point[Int]].coord(2) >= lby)
    }
  }

  def boundTest3DString(lbx: String, lby: String, lbz: String, ubx: String, uby: String, ubz: String) = {
    var searchSet = treeString.query(SpaceRegion(Point(-1, Array(lbx, lby, lbz)), Point(-1, Array(ubx, uby, ubz))))

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