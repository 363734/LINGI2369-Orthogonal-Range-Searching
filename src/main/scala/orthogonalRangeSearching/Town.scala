package orthogonalRangeSearching

import space._

case class Town(val id: Int, val name: String, val coordinate: (Double, Double))(implicit domainBound: (String, String)) extends SpacePoint {
  val dim: Int = 3

  def comparePoint(that: SpacePoint, dim: Int): Int = {
    val thatInstance = that.asInstanceOf[Town]
    if (dim == 1) {
      name.compareTo(thatInstance.name)
    } else if (dim == 2) {
      coordinate._1.compareTo(thatInstance.coordinate._1)
    } else {
      coordinate._2.compareTo(thatInstance.coordinate._2)
    }
  }

  def copyBut(point: SpacePoint, dim: Int): SpacePoint = {
    val thatInstance = point.asInstanceOf[Town]
    if (dim == 1) {
      Town(id, thatInstance.name, coordinate)
    } else if (dim == 2) {
      Town(id, name, (thatInstance.coordinate._1, coordinate._2))
    } else {
      Town(id, name, (coordinate._1, thatInstance.coordinate._2))
    }
  }

  def getFullSpace: SpaceRegion = {
    val lb = Town(-1, domainBound._1, (Double.MinValue, Double.MinValue))
    val ub = Town(-1, domainBound._2, (Double.MaxValue, Double.MaxValue))
    SpaceRegion(lb, ub)

  }

}