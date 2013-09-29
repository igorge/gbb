package gie.app.gbb.code.comet

import gie.app.gbb.model

import gie.app.gbb.templates._
import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._
import net.liftweb.http.{SHtml, CometListener, CometActor}
import net.liftweb.util.ClearClearable
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmds


class Chat extends CometActor with CometListener with Loggable { self =>
  private case class FullRender()

  private def t = loadTemplate("chat" :: "chat_view" :: Nil)

  private val chatLines = model.Chat.createLinesBuffer()


  def registerWith = model.Chat.mainChatRoom

  private def impl_Speak(msg: String) {
    val chatLine = model.Chat.ChatLine(model.Chat.NickName.is, msg)
    registerWith ! model.Chat.AddMsg( chatLine :: Nil)
  }
  private def impl_Spoken(msg: Seq[model.Chat.ChatLine]){
    logger.debug("Chat >>> " + msg)
    chatLines ++= msg

    self ! FullRender()
  }

  private def impl_resetChat(m: Seq[model.Chat.ChatLine]){
    chatLines.clear()
    chatLines ++= m

    self ! FullRender()
  }

  private def impl_buildFullUpdate(): xml.NodeSeq = {
    val builder = xml.NodeSeq.newBuilder
    chatLines.foreach{line=> builder += <li>{line.nick} : {line.data} </li>}
    builder.result()
  }

  val updateChatJs =
      """
            (function (){
              $('#linesContainer').scrollTop( $('#linesContainer')[0].scrollHeight );
              $('#chatTextBox').val('');
            })()
      """

  private def impl_doFullRender(){
    logger.debug(s"${self} >>> impl_doFullRender()")

    partialUpdate(
      SetHtml("chatLine", impl_buildFullUpdate()) & JsCmds.Run(updateChatJs)
    )
  }

  def render = {
    val css = "#chatTextBox" #> SHtml.onSubmit( impl_Speak _)

    val r = "#chatWindow" #> css.apply(t)

    self ! FullRender()

    r
  }

  override def lowPriority = {
    case model.Chat.FullSet(m) => impl_resetChat(m)
    case model.Chat.AddMsg(m) => impl_Spoken(m)
    case model.Chat.Noop() => logger.debug("NOOP")
    case FullRender() => impl_doFullRender()
  }

}