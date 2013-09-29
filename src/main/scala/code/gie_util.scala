package gie

import net.liftweb.common.LazyLoggable
import java.io.{FileInputStream, File}
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import scala.reflect.{ClassTag, classTag}


object Try {
  def nonfatal[T](allowed: ClassTag[_] *)(r: => T): scala.util.Try[T] =
    try Success(r) catch {
      case NonFatal(e) if allowed.find( _.runtimeClass.isInstance(e) ).isDefined => Failure(e)
    }

}

package object util extends LazyLoggable {

  implicit final class Pipe[T](val t:T) extends AnyVal{
    @inline def |%>[X, U](f: X=>U)(implicit ev: T=>X) = f(t)
    @inline def |>[U](f: T=>U) = f(t)
  }

  def file(root: java.io.File, leaf: String) = new java.io.File(root, leaf)
  def file(path: String) = new java.io.File(path)

  implicit class FileOps(val file: java.io.File) extends AnyVal {
    def / (leaf: String) = new File(file, leaf)
    def open_!() = new FileInputStream(file)
  }

  def protectWith[R](fun: =>R)(onExceptionFun: =>Unit):R = try {
    fun
  } catch {
    case e:Throwable =>
      val r = onExceptionFun
      throw e
  }

  def protectWith2[P, R](seedFunc: => P)(fun: P=>R)(onExceptionFun: P=>Unit):R = {
    val p = seedFunc
    try {
      fun(p)
    } catch {
      case e:Throwable =>
        val r = onExceptionFun(p)
        throw e
    }
  }

  def withRollback[P,R](initFun: =>P)(rollbackFun: P=>Unit)(protectedFun: P=>R):R = {
    val p = initFun
    try {
      protectedFun(p)
    } catch {
      case e:Throwable =>
        val r = rollbackFun(p)
        throw e
    }
  }


  class BoxToTryException(msg: String, cause: Throwable) extends Exception(msg, cause)
  class BoxEmptyException() extends BoxToTryException("net.liftweb.common.Box was empty", null)
  class BoxFailureException(msg: String, cause: Throwable = null) extends BoxToTryException(msg, cause)

  object Implicit {

    implicit class BoxToTry[T](val b: net.liftweb.common.Box[T]) extends AnyVal {
      def toTry: scala.util.Try[T] = b match {

        case net.liftweb.common.Full(v)   => scala.util.Success(v)
        case net.liftweb.common.Empty     => scala.util.Failure(new BoxEmptyException() )

        case net.liftweb.common.Failure(_ , net.liftweb.common.Full(e), _) => scala.util.Failure(e)
        case net.liftweb.common.Failure(msg, net.liftweb.common.Empty, net.liftweb.common.Full( net.liftweb.common.Failure(_, net.liftweb.common.Full(e), _  )) ) => scala.util.Failure( new BoxFailureException(msg, e) )
        case net.liftweb.common.Failure(msg, _, _) => scala.util.Failure( new BoxFailureException(msg) )
      }
    }

    implicit class TryExtensions[T](val t: scala.util.Try[T]) extends AnyVal {
      def fold[R](failureFun: scala.util.Failure[T]=>R)(successFun: T=>R) = t match {
        case scala.util.Success(v) => successFun(v)
        case err @ scala.util.Failure(_) => failureFun(err)
      }
    }

  }

}