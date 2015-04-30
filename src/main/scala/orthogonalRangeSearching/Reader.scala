package orthogonalRangeSearching

import scala.io.Source
import rangeTree._
import kdTrees._
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

    var kdTree = KdTree(points.toSet, 2)
    var rangeTree = RangeTree(points.toSet, 2)
    var fractionalTree = FractionalTree(points.toSet, 2)

    println(kdTree.searchKD(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500)))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500)))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array(1850, 0)), Point(-1, Array(1900, 500)))))

    // 3D
    val point3D = DataInt(0).zip(DataInt(1)).zip(DataInt(2)).map(x => Array(x._1._1, x._1._2, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    kdTree = KdTree(point3D.toSet, 3)
    rangeTree = RangeTree(point3D.toSet, 3)
    fractionalTree = FractionalTree(point3D.toSet, 3)

    println(kdTree.searchKD(SpaceRegion(Point(-1, Array(1850, 0, 1)), Point(-1, Array(1900, 500, 10)))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(1850, 0, 1)), Point(-1, Array(1900, 500, 10)))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array(1850, 0, 1)), Point(-1, Array(1900, 500, 10)))))

  }

  def bigData() = {
    val Data = read("car-fuel-and-emissions.csv", 45000)

    val DataString = Data._2.map(x => x)
    val points = DataString(0).zip(DataString(2)).map(x => Array(x._1, x._2)).zipWithIndex.map(x => Point(x._2, x._1))

    var kdTree = KdTree(points.toSet, 2)
    var rangeTree = RangeTree(points.toSet, 2)
    var fractionalTree = FractionalTree(points.toSet, 2)

    println(kdTree.searchKD(SpaceRegion(Point(-1, Array("datapartc_july2000.csv", "mitsubishi")), Point(-1, Array("datapartc_july2000.csv", "mitsubishi")))))
    println(rangeTree.rangeQuery(SpaceRegion(Point(-1, Array("datapartc_july2000.csv", "mitsubishi")), Point(-1, Array("datapartc_july2000.csv", "mitsubishi")))))
    println(fractionalTree.query(SpaceRegion(Point(-1, Array("datapartc_july2000.csv", "mitsubishi")), Point(-1, Array("datapartc_july2000.csv", "mitsubishi")))))

  }

  def cityData3D() = {
    val Data = read("worldcitiespop_UTF.txt", 20000)

    val names = Data._2(1).map { y => y }
    val lat = Data._2(5).map { y => y.toDouble }
    val long = Data._2(6).map { y => y.toDouble }

    val newPoints = names.zip(lat.zip(long)).zipWithIndex.map(x => Town(x._2, x._1._1, (x._1._2._1, x._1._2._2)))

    var time1 = System.currentTimeMillis

    var kdTree = KdTree(newPoints.toSet, 3)
    var time2 = System.currentTimeMillis
    println("kd build : " + (time2 - time1))
    val kdSearch = kdTree.searchKD(SpaceRegion(Town(-1, "", (40.0, 0.0)), Town(-1, "zzz", (55.0, 3.0))))
    var time3 = System.currentTimeMillis

    println("kd search size " + kdSearch.size)
    println("kd querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

    time1 = System.currentTimeMillis

    var rangeTree = RangeTree(newPoints.toSet, 3)
    time2 = System.currentTimeMillis
    println("range build : " + (time2 - time1))
    val rangeSearch = rangeTree.rangeQuery(SpaceRegion(Town(-1, "", (40.0, 0.0)), Town(-1, "zzz", (55.0, 3.0))))
    time3 = System.currentTimeMillis

    println("range search size " + rangeSearch.size)
    println("range querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

    time1 = System.currentTimeMillis

    var fracTree = FractionalTree(newPoints.toSet, 3)
    time2 = System.currentTimeMillis
    println("fract build : " + (time2 - time1))
    val fracSearch = fracTree.query(SpaceRegion(Town(-1, "", (40.0, 0.0)), Town(-1, "zzz", (55.0, 3.0))))
    time3 = System.currentTimeMillis

    println("fract search size " + fracSearch.size)
    println("fract results : " + fracSearch)
    println("fract querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

  }

  def cityData2D() = {
    val Data = read("worldcitiespop_UTF.txt", 200000)

    val names = Data._2(1).map { y => y }
    val lat = Data._2(5).map { y => y.toDouble }
    val long = Data._2(6).map { y => y.toDouble }

    val newPoints = lat.zip(long).zipWithIndex.map(x => Point(x._2, Array(x._1._1, x._1._2)))

    var time1 = System.currentTimeMillis

    var kdTree = KdTree(newPoints.toSet, 2)
    var time2 = System.currentTimeMillis
    println("kd build : " + (time2 - time1))
    val kdSearch = kdTree.searchKD(SpaceRegion(Point(-1, Array(30.0, 1.0)), Point(-1, Array(60.0, 10.0))))
    var time3 = System.currentTimeMillis

    println("kd search size " + kdSearch.size)
    println("kd querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

    time1 = System.currentTimeMillis

    var rangeTree = RangeTree(newPoints.toSet, 2)
    time2 = System.currentTimeMillis
    println("range build : " + (time2 - time1))
    val rangeSearch = rangeTree.rangeQuery(SpaceRegion(Point(-1, Array(30.0, 1.0)), Point(-1, Array(60.0, 10.0))))
    time3 = System.currentTimeMillis

    println("range search size " + rangeSearch.size)
    println("range querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

    time1 = System.currentTimeMillis

    var fracTree = FractionalTree(newPoints.toSet, 2)
    time2 = System.currentTimeMillis
    println("frac build : " + (time2 - time1))
    val fracSearch = fracTree.query(SpaceRegion(Point(-1, Array(30.0, 1.0)), Point(-1, Array(60.0, 10.0))))
    time3 = System.currentTimeMillis

    println("frac search size " + fracSearch.size)
    //    println("frac results : " + fracSearch)
    println("frac querry : " + (time3 - time2))
    println("Total : " + (time3 - time1))

  }

  //  smallData()
  //  bigData()
  //  cityData2D()
  cityData3D()
}
