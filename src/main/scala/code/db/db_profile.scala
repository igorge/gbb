package gie.app.gbb

import scala.slick.jdbc.StaticQuery
import scala.slick.session.Session

//////////////////////////////////////////////////////////////////////////////////

trait DBUtil {
  def setSerializableForTransaction()(implicit session: Session):Unit
}

private object H2DriverUtils extends DBUtil {
  def setSerializableForTransaction()(implicit session: Session){
    val q = StaticQuery.updateNA("SET LOCK_MODE 1;")
    q.execute()
  }
}

object SlickProfile {
  import scala.slick.driver._

  val profile: ExtendedProfile = scala.slick.driver.H2Driver
  val utils:DBUtil = H2DriverUtils
}

//////////////////////////////////////////////////////////////////////////////////

