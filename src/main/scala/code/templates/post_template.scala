package gie.app.gbb.templates

import gie.app.gbb.model.FileRecordResolver
import net.liftweb.http.{RequestVar, SHtml, Templates}
import net.liftweb.common.{Empty, LazyLoggable}
import net.liftweb.util.Helpers._

import gie.app.gbb.{model, db}
import net.liftweb.util.{CssSel}
import org.joda.time.DateTimeZone

//////////////////////////////////////////////////////////////////////////////////
private object CommonPost extends LazyLoggable {

  private val impl_videoIconUrl = "/images/video_icon.png"
  private val impl_audioIconUrl = "/images/audio_icon.png"

  private def impl_getIconUrl(fileRecord: db.FileRecord): Option[String] = {
    fileRecord.mime.split('/') match {
      case Array("video", _) => Some(impl_videoIconUrl)
      case Array("audio", _) => Some(impl_audioIconUrl)
      case _ => None
    }

  }

  def bindPost(p: db.Message, fileRecords: FileRecordResolver): CssSel = {

    def lookupFile(fileId: Long) = {
      val mainFileRecord = fileRecords(fileId)
      val thumbnailMaybe = for (fr          <- mainFileRecord;
                                thumbId     <- fr.thumbnailId;
                                thumbRecord <- fileRecords(thumbId) ) yield thumbRecord

      (mainFileRecord, thumbnailMaybe)
    }

    "#threadPostId [id]" #> s"post${p.id.get.toString}" &
    "#postSubject" #> p.subject &
    "#postId" #> p.id.get.toString() &
    "#email" #> p.email.map(xml.Text(_)).getOrElse(anonXml) &
    "#comment" #> model.Board.processComment( p.comment ) &
    "#postTime" #> p.cDate.withZone(DateTimeZone.UTC).toString("yyyy.MM.dd (E) HH:mm:ss") &
    "#postsCount" #> p.childCount &
    "#fileId" #>
      p.file.fold[xml.NodeSeq]{ xml.NodeSeq.Empty } { fileId=>
        val (fileMaybe, thumbnail) = lookupFile(p.file.get)
        fileMaybe.fold{ logger.error(s"Not found file record with id='${fileId}' in cache"); xml.NodeSeq.Empty } { file=>
          val thumbUrl:Option[String] = thumbnail.map(model.Board.urlForThumbnail) orElse {impl_getIconUrl(file)}
          <a href={model.Board.urlForFile(file)}>{ thumbUrl.fold[xml.NodeSeq]{xml.Text(file.uuid.toString) }{url=> <img src ={url}> </img> } }</a>
        }
      }

  }


}


object post extends LazyLoggable {

  private val TEMPLATE_POST_VIEW = "post" :: "post_view" :: Nil
  private def postTemplate = loadTemplate(TEMPLATE_POST_VIEW)

  def bind(p: Seq[db.Message], fileRecords: FileRecordResolver): xml.NodeSeq = {
    val b = "#threadPostsContainerId" #> ( "#threadPostId" #> p.map{ bindPost(_, fileRecords) })
    b(postTemplate)
  }

  def bind(p: db.Message, fileRecords: FileRecordResolver): xml.NodeSeq = {
    ("#threadPostsContainerId [id]" #> (None:Option[String]) & bindPost(p, fileRecords)).apply(postTemplate)
  }


  private def bindPost(p: db.Message, fileRecords: FileRecordResolver): CssSel = CommonPost.bindPost(p, fileRecords)


}


package object board extends LazyLoggable {
  private def postTemplate = loadTemplate("post" :: "board_post_view" :: Nil)
  private def topicViewTemplate = loadTemplate("post" :: "board_topic_view" :: Nil)

  def bind(p: Seq[db.Message], fileRecords: FileRecordResolver): xml.NodeSeq = {
    val b = "#threadPostsContainerId" #> ( "#threadPostId" #> p.map{ bindPost(_, fileRecords) })
    b(postTemplate)
  }

  def bindWithChild(p: Array[(db.Message, Array[db.Message])], fileRecords:FileRecordResolver ): xml.NodeSeq = {
    val b = "#threadPostsContainerId" #> p.map{ topic=>
      bindPost(topic._1, fileRecords) &
      "#repliesContainer" #> (
        if(topic._2.isEmpty)
          "*" #> {n:xml.NodeSeq => xml.NodeSeq.Empty}
        else
          "#topicReply" #> {n:xml.NodeSeq => post.bind(topic._2, fileRecords) }
      )
    }
    b(topicViewTemplate)
  }

  private def bindPost(p: db.Message, fileRecords: FileRecordResolver): CssSel = {
    CommonPost.bindPost(p, fileRecords) &
    "#viewThread [href]" #> model.Board.urlForPost(p)
  }

}


package board_post_form {

import net.liftweb.http.FileParamHolder

case class FormData(
            fileHolder : Option[FileParamHolder] = None,
            subject: String = "",
            email: String = "",
            comment: String = "")
}

package object board_post_form extends LazyLoggable {


  private val TEMPLATE_POST_VIEW = "post" :: "boad_post_form" :: Nil
  private def t = loadTemplate(TEMPLATE_POST_VIEW)

  def bind(onSubmitFun: FormData=>Unit): xml.NodeSeq = {

    object form extends RequestVar[FormData](FormData())

    def impl_checkSizes(){
      if (form.is.subject.length>1024) model.Board.abort.invalidFormComponentSize(form.is.comment.length)
      if (form.is.email.length>256) model.Board.abort.invalidFormComponentSize(form.is.comment.length)
      if (form.is.comment.length>4*1024) model.Board.abort.invalidFormComponentSize(form.is.comment.length)
    }

    def submit(){
      impl_checkSizes()
      onSubmitFun( form )
    }

    val css =
      "#subjectBox" #> SHtml.text(form.subject, v=>form( form.is.copy(subject=v) ) ) &
      "#emailBox"   #> SHtml.text(form.email, v=>form( form.is.copy(email=v) ) ) &
      "#commentBox" #> SHtml.textarea(form.comment, v=>form ( form.is.copy(comment=v)) ) &
      "#imageUploadId"    #> SHtml.fileUpload{v=>form ( form.is.copy( fileHolder = Some(v) ) ) } &
      "type=submit" #> SHtml.onSubmitUnit(submit)


    css.apply(t)
  }


}

