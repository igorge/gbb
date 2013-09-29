package gie.app.gbb.code.snippet
//////////////////////////////////////////////////////////////////////////////////
import gie.app.gbb.model._

import net.liftweb.common.{Loggable, Box,Empty,Full}
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import scala.xml.Text

//////////////////////////////////////////////////////////////////////////////////
class BoardsList extends Loggable {

  def render = {
    "#boardId" #> Board.boards.map{ b=>
      "#title"        #> <a href={Board.urlPath(b)}> {Text( b.title )} </a> &
      "#description"  #> b.description
    }
  }


}
