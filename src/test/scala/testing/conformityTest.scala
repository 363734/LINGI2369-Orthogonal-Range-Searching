package testing

import scala.io.Source
import org.scalatest._
import rangeTree._
import kdTrees._
import fractionalCascading._
import space._

/**
 * Class testing that the searches on the 3 different techniques return the same results
 */
class conformityTest extends FlatSpec {

  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  implicit val boundString = ("", "zzzzz")

  /**
   * Reads the file and return a 2 dimension array
   */
  def read(arg: String): (Int, Array[Int], Array[Int]) = {

    // Import the file
    val source = Source.fromFile(arg)
    // Get the lines in an iterator
    val lines = source.getLines()
    val line1 = lines.next.split(",")
    val n = 260

    var dim1 = new Array[Int](n)
    var dim2 = new Array[Int](n)

    for (i <- 0 until n) {
      val line = lines.next.split(",")
      dim1(i) = line(0) toInt;
      dim2(i) = line(1) toInt;
    }
    (n, dim1, dim2)
  }

  // Reading from the data
  val Data = read("co2-fossil-global.csv")
  val points = Data._2.zip(Data._3).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  // Making the different trees
  val kdTree = KdTree(points.toSet, 2)
  val rangeTree = RangeTree(points.toSet, 2)
  val fractionalTree = FractionnalTree(points.toSet, 2)

  val kdSearch = kdTree.searchKD(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500))))
  val rangeSearch = rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500))))
  val fractSearch = fractionalTree.query(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500))))

  val kdList = kdSearch.toList
  val rangeList = rangeSearch.toList
  val fractList = fractSearch.toList

  "The size of results " should "be the same " in {
    assert(rangeSearch.size == kdSearch.size)
    assert(rangeSearch.size == fractSearch.size)
  }

  "The results " should "be the same " in {
    for (i <- 0 until rangeList.size) {
      assert(kdList.contains(rangeList(i)))
      assert(fractList.contains(rangeList(i)))
    }

  }

}