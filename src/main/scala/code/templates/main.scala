package gie.app.gbb

import net.liftweb.http.Templates
import net.liftweb.common.LazyLoggable

package object templates extends LazyLoggable {

  val TEMPLATE_ROOT = "templates-hidden"
  val templateRoot = TEMPLATE_ROOT

  def loadTemplate(path: List[String]) = {
    Templates( templateRoot :: path ) openOr {
      logger.error(s"Failed to load template: ${path} at ${templateRoot}")
      throw new TemplateNotFound(templateRoot :: path)
    }
  }

  def anonXml: xml.NodeSeq = xml.Text("Anonymous")


  class TemplateNotFound(val path:Seq[String]) extends NoSuchElementException(path.toString())


}