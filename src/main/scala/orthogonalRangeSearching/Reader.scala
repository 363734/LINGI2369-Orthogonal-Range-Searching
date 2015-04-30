package orthogonalRangeSearching

import scala.io.Source
import rangeTree._
//import kdTrees._
import fractionalCascading._
import space._

/**
 * Read the file and return the data from it
 */
object Reader extends App {

  implicit val boundInt = (Int.MinValue, Int.MaxValue)
  implicit val boundString = ("", "zzzzz")
  implicit val boundDouble = (Double.MinValue, Double.MaxValue)

  /**
   * Reads the file and return a 2 dimension array
   */
  def read(arg: String, n: Int): (Int, Array[Array[String]]) = {

    // Import the file
    val source = Source.fromFile(arg)
    // Get the lines in an iterator
    val lines = source.getLines()

    val line1 = lines.next.split(",")

    var dim1 = new Array[String](n)
    var dim2 = new Array[String](n)
    var dim3 = new Array[String](n)
    var dim4 = new Array[String](n)
    var dim5 = new Array[String](n)
    var dim6 = new Array[String](n)
    var dim7 = new Array[String](n)

    for (i <- 0 until n) {
      val line = lines.next.split(",")
      try {
        dim1(i) = line(0).toLowerCase();
        dim2(i) = line(1).toLowerCase();
        dim3(i) = line(2).toLowerCase();
        dim4(i) = line(3).toLowerCase();
        dim5(i) = line(4).toLowerCase();
        dim6(i) = line(5).toLowerCase();
        dim7(i) = line(6).toLowerCase();
      }

    }

    (n, Array(dim1, dim2, dim3, dim4, dim5, dim6, dim7))
  }

  def smallData() = {
    val Data = read("co2-fossil-global.csv", 260)

    val DataInt = Data._2.map(x => x.map { y => y.toInt })
    // 2D
    val points = DataInt(0).zip(DataInt(1)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    //    var kdTree = KdTree(points.toSet, 2)
    var rangeTree = RangeTree(points.toSet, 2)
    var fractionalTree = FractionnalTree(points.toSet, 2)

    //    println(kdTree.searchKD(SpaceRegion(Array(Some(1850), Some(0)), Array(Some(1900), Some(500)))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500)))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500)))))

    // 3D
    val point3D = DataInt(0).zip(DataInt(1)).zip(DataInt(2)).map(x => Array(x._1._1, x._1._2, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    //    kdTree = KdTree(point3D.toSet, 3)
    rangeTree = RangeTree(point3D.toSet, 3)
    fractionalTree = FractionnalTree(point3D.toSet, 3)

    //    println(kdTree.searchKD(SpaceRegion(Array(Some(1850), Some(0), Some(1)), Array(Some(1900), Some(500), Some(10)))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(1850, 0, 1)), Point(-1, Array(1900, 500, 10)))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array(1850, 0, 1)), Point(-1, Array(1900, 500, 10)))))

  }

  def bigData() = {
    val Data = read("car-fuel-and-emissions.csv", 45000)

    val DataString = Data._2.map(x => x)
    val points = DataString(0).zip(DataString(2)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    //    var kdTree = KdTree(points.toSet, 2)
    var rangeTree = RangeTree(points.toSet, 2)
    var fractionalTree = FractionnalTree(points.toSet, 2)

    //    println(kdTree.searchKD(SpaceRegion(Array(Some("datapartc_july2000.csv"), Some("mitsubishi")), Array(Some("datapartc_july2000.csv"), Some("mitsubishi")))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array("datapartc_july2000.csv", "mitsubishi")), Point(-1, Array("datapartc_july2000.csv", "mitsubishi")))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array("datapartc_july2000.csv", "mitsubishi")), Point(-1, Array("datapartc_july2000.csv", "mitsubishi")))))

  }

  def cityData() = {
    val Data = read("worldcitiespop_UTF.txt", 200000)

    val lat = Data._2(5).map { y => y.toDouble }
    val long = Data._2(6).map { y => y.toDouble }
    val points = lat.zip(long).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    var time = System.currentTimeMillis

    //    var kdTree = KdTree(points.toSet, 2)
    //    println(points(1))
    //    println("kd build done")
    //    val kdSearch = kdTree.searchKD(SpaceRegion(Array(Some(40.0), Some(0.0)), Array(Some(55.0), Some(3.0))))
    //    println("kd search size " + kdSearch.size)

    println(System.currentTimeMillis - time)

    time = System.currentTimeMillis

    var rangeTree = RangeTree(points.toSet, 2)
    println("range build done")
    val rangeSearch = rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(40.0, 0.0)), Point(-1, Array(55.0, 3.0))))
    println("range search size " + rangeSearch.size)

    println(System.currentTimeMillis - time)

    time = System.currentTimeMillis

    var fractionalTree = FractionnalTree(points.toSet, 2)
    println("frac build done")
    val fracSearch = fractionalTree.query(SpaceRegion(Point(-1, Array(40.0, 0.0)), Point(-1, Array(55.0, 3.0))))
    println("frac search size " + fracSearch.size)
    println("range search size " + rangeSearch.size)

    println(System.currentTimeMillis - time)
  }

  //  smallData()
  //  bigData()
  cityData()
}
