package gie.app.gbb.model

import net.liftweb.actor.LiftActor
import net.liftweb.http.{SessionVar, ListenerManager}
import com.twitter.util.RingBuffer
import net.liftweb.common.Loggable

package object Chat {

  def urlPath = "/chat"

  object NickName extends SessionVar[String](impl_genName())

  private object ChatUserId extends (()=>Long) {
    var current: Long = 0
    def apply() = synchronized {
      val c = current
      current += 1
      c
    }
  }

  case class ChatLine(nick: String, data: String)

  sealed trait Msg
  case class Noop() extends Msg
  case class AddMsg(m:Seq[ChatLine]) extends Msg
  case class FullSet(m:Seq[ChatLine] ) extends Msg

  val MAX_CHAT_LINES = 100
  val MAX_CHAT_LINE_LENGTH = 256

  def createLinesBuffer() = new RingBuffer[ChatLine](MAX_CHAT_LINES)
  private def impl_genName() = s"Anon${ChatUserId()}"

  class ChatRoomServer extends LiftActor with ListenerManager with Loggable {

    private val chatLine = createLinesBuffer()

    private def impl_truncateLine(msg: ChatLine) = {
      if(msg.data.length<=MAX_CHAT_LINE_LENGTH) msg else msg.copy( data = msg.data.substring(0, MAX_CHAT_LINE_LENGTH) )
    }

    private def impl_addMessages(origm: Seq[ChatLine]) = if (!origm.isEmpty) {

      val m = origm.filter(!_.data.isEmpty).map(impl_truncateLine (_))

      if(!m.isEmpty){
        m.foreach(chatLine += _)
        updateListeners( AddMsg(m) )
      }

    }

    def createUpdate:Msg = {
        if(chatLine.length==0) Noop() else FullSet( chatLine.to[Array] )
    }

    override def lowPriority = {
      case AddMsg(m) =>
        impl_addMessages(m)
    }

  }

  object mainChatRoom extends ChatRoomServer

}


