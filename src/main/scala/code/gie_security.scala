package gie

import net.liftweb.common.{Loggable}
import org.bouncycastle.openssl
import net.liftweb.util.Props
import java.io._
import org.bouncycastle.jce.provider.{X509CertificateObject, BouncyCastleProvider}
import java.security.{Signature, MessageDigest}
import javax.mail.internet.{MimeMultipart, MimeMessage}
import org.bouncycastle.util.encoders.Base64

package security {

  class SecurityException extends Exception
  class AccessDeniedException(userUUID: gie.UUID) extends SecurityException
  class PrivilegeNotHeldException(userUUID: gie.UUID, required: UserPrivilege) extends AccessDeniedException(userUUID)

  trait UserPrivilege
  trait UserToken {
    def userId: gie.UUID
    def privileges: Set[UserPrivilege]
  }
  case class ResourceAccessToken(requiredPrivileges : Set[UserPrivilege]) extends AnyVal

  package privileges {
    object PRIV_ROOT extends UserPrivilege
  }


}


package object security extends Loggable{
  val systemUUID = gie.UUID.zero
  val guestUUID = gie.UUID(Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0,1,3,3,7))

  def requirePrivileges(user: UserToken, requiredPrivileges: Seq[UserPrivilege]){
    if( user.userId == systemUUID) {
      logger.info(s"SYSTEM UUID, ignoring checks for ${user}, required resource access privileges: ${requiredPrivileges}")
    } else {
      val userPrivs = user.privileges
      requiredPrivileges.foreach{ priv =>
        if(!userPrivs.contains(priv)) throw new PrivilegeNotHeldException(user.userId, priv)
      }
    }
  }
}



