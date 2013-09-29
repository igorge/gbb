package gie.app.gbb.code.snippet

import net.liftweb.common.{Loggable, Box,Empty,Full}
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import gie.app.gbb.model

class BecomeAdmin extends Loggable {


  def impl_processSignature(password: String){
    if( model.security.isAdmin(password) ){
      model.security.authenticateAsAdmin()
    } else {
      logger.warn(s"!!! AUTH as admin with '${password}' have failed !!!")
    }
  }

  def render = {
    if(model.security.isAdminSession())
      "fieldset *" #> xml.NodeSeq.Empty
    else
      "#adminPassword" #> SHtml.onSubmit( impl_processSignature _ )
  }


}