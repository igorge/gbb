package gie.app.gbb.model

import net.liftweb.http.S
import net.liftweb.common.Loggable
import net.liftweb.util.Props

package i2p {

import org.bouncycastle.util.encoders.{Base64, Hex}

case class DestHash(data: Array[Byte], trusted: Boolean){
    override def toString = {
      s"DestHash(${ Base64.encode(data).foldLeft(new StringBuilder)(_ append  _.toChar).result() }, ${trusted})"
    }
  }
}


package object i2p extends Loggable {
  import org.bouncycastle.util.encoders.{Base64, Hex}
  import java.security.{Signature, MessageDigest}

  val DEST_HASH = "X-I2P-DestHash"
  val FORWARDED_SERVER = "HTTP_X_FORWARDED_SERVER"
  val FORWARDED_FOR = "HTTP_X_FORWARDED_FOR"

  lazy val dummyDesthHash = {
    val dest = "VOID NON VALID DEST"
    val digestEngine = MessageDigest.getInstance("SHA256")
    digestEngine.update(dest.getBytes("UTF-8"))
    digestEngine.digest()
  }

  def getDestHashFromReq():Option[DestHash] = Props.mode match {
    case Props.RunModes.Development =>
      val dh = Some(DestHash(dummyDesthHash, true))
      logger.warn(s"!!! DEVELOPER MODE: returning dummy ${DEST_HASH}: ${dh} !!!")
      dh
    case _ =>
      S.param(DEST_HASH).toOption.map{ dh=>
        val rawDh = Base64.decode(dh)
        logger.debug(s"${DEST_HASH}: ${Hex.encode(rawDh).foldLeft(new StringBuilder){_ append _.toChar}.result()}")
        DestHash(rawDh, forwarderHeaders_?)
      }
  }


  def forwarderHeaders_?() = {
    S.param(FORWARDED_SERVER).isDefined || S.param(FORWARDED_FOR).isDefined
  }

}




