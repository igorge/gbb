package gie.app.gbb.code.snippet

import net.liftweb.util.Helpers._
import gie.app.gbb.model
import net.liftweb.common.{Empty, Loggable}
import net.liftweb.http._


case class ExportErrorResponse(message: String) extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(message.getBytes("UTF-8"), "Content-Type" -> "text/plain; charset=utf-8" :: headers, cookies, 400)
}

case class ExportResponse(message: xml.NodeSeq) extends LiftResponse with HeaderDefaults {
  private def impl_data(m: xml.NodeSeq) = (<html><body>{m}</body></html>).toString().getBytes("UTF-8")
  def toResponse = InMemoryResponse( impl_data(message), "Content-Type" -> "text/html; charset=utf-8" :: headers, cookies, 200)
}


object wrapImportExportResponse {
  def apply(fun: =>Unit){
    try {fun} catch {
      case e:model.import_export.ExportException =>
        throw new ResponseShortcutException(()=>new ExportErrorResponse(e.toString), Empty, false)
    }
    throw new ResponseShortcutException(()=>new ExportResponse(xml.Text("OK")), Empty, false)
  }
}


class Export extends Loggable {


  def render = {

    var backupTargetPath =""

    def submit(){
      wrapImportExportResponse{ model.import_export.export(backupTargetPath) }
    }

    "#exportPath" #> SHtml.text(backupTargetPath, backupTargetPath = _) &
    "type=submit" #> SHtml.onSubmitUnit(submit)
  }


}

class Import extends Loggable {


  def render = {

    var backupTargetPath =""

    def submit(){
      wrapImportExportResponse{ model.import_export.import_(backupTargetPath) }
    }

    "#exportPath" #> SHtml.text(backupTargetPath, backupTargetPath = _) &
    "type=submit" #> SHtml.onSubmitUnit(submit)
  }


}
