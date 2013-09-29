package gie.app.gbb.model

import net.liftweb.common.{Full, Empty, Loggable}
import net.liftweb.util.LoanWrapper
import org.bouncycastle.openssl
import net.liftweb.util.Props
import java.io._
import scala.util.Try
import org.bouncycastle.jce.provider.{X509CertificateObject, BouncyCastleProvider}
import java.security.{Signature, MessageDigest}
import javax.mail.internet.{MimeMultipart, MimeMessage}
import gie.util.Implicit._
import org.bouncycastle.util.encoders.Base64
import net.liftweb.http._
import scala.Some

package security {
  class UserToken(val userId: gie.UUID, val privileges: Set[gie.security.UserPrivilege]) extends gie.security.UserToken

  package privileges {
    object PRIV_VIEW_BOARD extends gie.security.UserPrivilege
    object PRIV_POST_REPLY extends gie.security.UserPrivilege
    object PRIV_START_THREAD extends gie.security.UserPrivilege
  }

}


package object security extends Loggable {


  object currentUser extends SessionVar[UserToken](null)

  object AnonymousUser extends UserToken(gie.security.guestUUID, Set(privileges.PRIV_VIEW_BOARD, privileges.PRIV_POST_REPLY, privileges.PRIV_START_THREAD) )
  object SystemUser extends UserToken(gie.security.systemUUID, Set(gie.security.privileges.PRIV_ROOT) )

  def requirePrivileges(privileges: gie.security.UserPrivilege*){
    val user = currentUser.performAtomicOperation{ if(currentUser.set_?) currentUser.get else AnonymousUser }
    gie.security.requirePrivileges(user, privileges)
  }

  def privileges_?(privileges: gie.security.UserPrivilege*) = try {
    requirePrivileges(privileges :_* )
    true
  } catch {
    case e:gie.security.SecurityException => false
  }

  private def impl_installExceptionFilter(){
      //type handlerParams = (net.liftweb.util.Props.RunModes.Value, net.liftweb.http.Req, Throwable)

      def impl_Forbidden() = {
        logger.debug("Translated security exception")
        ForbiddenResponse()
      }

      LiftRules.exceptionHandler prepend {
        case (_, _, e: java.lang.reflect.InvocationTargetException) if e.getTargetException.isInstanceOf[gie.security.SecurityException] =>
          impl_Forbidden()
        case (_, _, e: gie.security.SecurityException ) =>
          impl_Forbidden()
      }
  }

  private def impl_installURLSecurityHooks(){

    LiftRules.dispatch.prepend{
      case Req("admin" :: rest, suffix, requestType) if(!privileges_?(gie.security.privileges.PRIV_ROOT)) =>
        ()=>Full(ForbiddenResponse())
    }
  }

  private def impl_authAsAdminForDebug(){
    def impl_liftHook_afterSessionCreate(session: LiftSession, req: Req){
      authenticateAsAdmin()
    }

    LiftSession.afterSessionCreate = impl_liftHook_afterSessionCreate _ :: LiftSession.afterSessionCreate

  }

  def secProvName = BouncyCastleProvider.PROVIDER_NAME
  private val PASSWORD_KEY_NAME = "admin_password_sha256"

  def boot() = {
    impl_installExceptionFilter()
    impl_installURLSecurityHooks()
    //impl_authAsAdminForDebug()

    if( java.security.Security.getProvider(secProvName) eq null) {
      logger.debug(s"Registering '${secProvName}'")
      java.security.Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())
      Some(()=>{
        java.security.Security.removeProvider(secProvName)
      })
    } else {
      logger.debug(s"'${secProvName}' already registered")
      None
    }
  }

  def authenticateAsAdmin(){
    logger.warn("!!! Authenticating session as admin !!!")
    currentUser.atomicUpdate{oldToken =>
      logger.debug(s"Old session's token ${oldToken}")
      SystemUser
    }
  }


  def isAdminSession()={
    currentUser.set_? && (currentUser.get ne null) && (currentUser.get.userId == SystemUser.userId)
  }

  def isAdmin(password: String): Boolean = {
    import org.bouncycastle.util.encoders.Hex
    Props.get(PASSWORD_KEY_NAME).toTry.flatMap{storePasswordHex=>Try{
      val storedPasswordSha = Hex.decode(storePasswordHex)
      val shaEngine = MessageDigest.getInstance("SHA256", secProvName)
      shaEngine.update(password.getBytes("UTF-8"))
      val sha = shaEngine.digest()
      storedPasswordSha.toSeq == sha.toSeq
    }}.fold{failure=>
      logger.debug(s"Admin password verification failed because of ${failure.exception.getStackTraceString}")
      false
    }{v=>
      logger.debug(s"Admin password verification result: ${v}")
      v
    }

  }


  def test() = try {
  } catch {
    case e: Throwable => e.printStackTrace()
  }

}



