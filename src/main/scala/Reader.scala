package OrthogonalRangeSearching

import scala.io.Source
import rangeTree._
import kdTrees._

/**
 * Read the file and return the data from it
 */
object Reader extends App {

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

  val Data = read("global.csv")

  val points = Data._2.zip(Data._3).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

  val kdTree = KdTree(points.toSet, 2)
  println(kdTree.value)
  val rangeTree = RangeTree(points.toSet, 2)

  println(kdTree.searchKD(SpaceRegion(Array(Some(1850), Some(0)), Array(Some(1950), Some(500)))))
  println(rangeTree.rangeQuery(SpaceRegion(Array(Some(1900), Some(0)), Array(Some(1950), Some(500)))))
}