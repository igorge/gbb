package gie

import com.github.nscala_time.time.Imports.DateTime
//import scala.slick.driver.H2Driver.simple._
import scala.slick.lifted.MappedTypeMapper.base
import scala.slick.lifted.TypeMapper
//////////////////////////////////////////////////////////////////////////////////
object DateTimeMapper {

  def map(t:DateTime) = new java.sql.Timestamp(t.getMillis)

  implicit val date2dateTime = base[DateTime, java.sql.Timestamp] (
    dateTime => new java.sql.Timestamp(dateTime.getMillis),
    date => new DateTime(date)
  )

}
//////////////////////////////////////////////////////////////////////////////////
