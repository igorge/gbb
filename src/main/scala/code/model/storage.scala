package gie.app.gbb.model
//////////////////////////////////////////////////////////////////////////////////

import gie.app.gbb._
import gie.io.WithIS
import gie.UUID
import java.io._
import gie.io.WithIS
import net.liftweb.common.{Full, Empty, LazyLoggable, Loggable}
import javax.imageio.ImageIO
import org.imgscalr.Scalr
import net.liftweb.http.rest.RestHelper
import net.liftweb.http._
import scala.Some
import scala.Some

//////////////////////////////////////////////////////////////////////////////////
object FileServe extends RestHelper with LazyLoggable {

  val fileServeTransactionTimeOut = 10*60

  def boot() = {
    LiftRules.dispatch.append(FileServe)
    None
  }

  val tika = new org.apache.tika.Tika()

  serve {
    case req @ Req("img" :: gie.UUID(id) :: Nil, "", GetRequest) => try_to_serve_file(req, id)
  }

  private def impl_doServeFile(id: gie.UUID, lazyIs: ()=>InputStream)(os:java.io.OutputStream):Unit = try {
    resource.managed{ lazyIs() }.acquireAndGet{is=>
      logger.debug(s"Serving file ${id}")

      gie.io.Helpers.copy(is, os, 4*1024)

      logger.debug(s"Serving of file ${id} have finished")
    }

  } catch {
    case e:java.io.EOFException =>
      logger.debug(s"Connection terminated before file have been served")
    case e:Throwable =>
      logger.error(s"Exception while serving file: ${e} at:\n${e.getStackTraceString}")
      throw e
  }

  def try_to_serve_file(req: Req, id: gie.UUID): LiftResponse = {

    //Coordinator.requireReadOnlyTransaction()

    FileRecord(id).fold[LiftResponse](NotFoundResponse()){ fileRecord=>
      logger.debug(req.headers)

      req.header("If-None-Match").flatMap{ eTag => if(eTag == id.toString()) Full(eTag) else Empty}.toOption.fold[LiftResponse]{
        Coordinator.requireNewReadOnlyTransaction(fileServeTransactionTimeOut) // new transaction so we will free slick db locks

        FS.openFile(id).fold[LiftResponse](NotFoundResponse()){ lazyBaseIs=>

          val tooLazyIs = ()=>new BufferedInputStream(lazyBaseIs())

          logger.debug(s"FILE: '${id}', detected file type: ${fileRecord.mime}")

          val headers =
            ("Content-Type" -> (fileRecord.mime)) ::
            ("Cache-Control" -> ("max-age=2592000")) ::
            ("ETag" -> s"${fileRecord.uuid.toString()}") :: Nil

          OutputStreamResponse(impl_doServeFile(id, tooLazyIs) _, headers)
        }
      }{ eTag:String  =>
        logger.debug("Got Etag, returning [304]")
        InMemoryResponse(Array(), "ETag" -> s"${eTag}" :: Nil, Nil, 304)
      }
    }

  }
}
//////////////////////////////////////////////////////////////////////////////////


object FileStore extends Loggable {

  def thumbnailSize = (200,200)

  private val tika = new org.apache.tika.Tika()

  private def impl_withNewFile(fun: OutputStream=>Unit): (UUID) = {
    val tmp = FS.createFile()
    fun( tmp._1 )
    tmp._1.flush()
    tmp._2
  }

    private def impl_storeFile(is: InputStream): (UUID, Option[(String,String)]) = {
      assume(is.markSupported())

      val fileMeta = FS.createFile()

      (for(os <- resource.managed(fileMeta._1)) yield {
        val fType =  impl_detectMimeType(is)
        gie.io.Helpers.copy(is, os)
        (fileMeta._2, fType)
      }).opt.get

    }

  private def impl_detectMimeType(is: InputStream): Option[(String, String)] = {
    assume(is.markSupported())

    val detectedType = tika.detect(is)
    logger.debug("Tika file type detected: "+detectedType)
    val splt = detectedType.split('/')
    if(splt.size!=2) None else Some(splt(0), splt(1))
  }

  private def impl_generateThumbnail(fileId: UUID): Option[(UUID, (String, String))] = WithIS(FS.openFile(fileId).get){is=>
    val size = thumbnailSize

    val thumbnailMaybe = for( bufferedImage <- Option( ImageIO.read(is) ) ) yield { Scalr.resize(bufferedImage, size._1, size._2) }
    thumbnailMaybe.map{ tn=>
      val id = impl_withNewFile{ os=>ImageIO.write(tn, "png", os) }
      (id,("image","png"))
    }
  }

  private def isImageType(ftype: (String, String)) = ftype._1 == "image"
  private def impl_mimeToString(m: Option[(String, String)]) = m.map{v=> s"${v._1}/${v._2}" }.getOrElse("application/octet-stream")

  private def impl_createRecordForFile(id: UUID, mime: String, thumbnailId: Option[Long]) ={
    db.FileRecord(
      uuid = id,
      mime = mime,
      thumbnailId = thumbnailId
    )
  }
  private def impl_storeRecordForFile(id: UUID, mime: String, thumbnailId: Option[Long]) = {
    val record = impl_createRecordForFile(id, mime, thumbnailId)
    DB.data.insertNewFileRecord(record)
  }

  def store(is: InputStream): db.FileRecord = {

    val fileId = impl_storeFile(is)

    val thumbnailIdMaybe = for(
      fType <- fileId._2 if isImageType(fType);
      id    <- impl_generateThumbnail(fileId._1)
    ) yield (id)

    val thumbnailRecordMaybe = thumbnailIdMaybe map {id=>
      impl_storeRecordForFile(id._1, impl_mimeToString( Some(id._2) ), None)
    }
    impl_storeRecordForFile(fileId._1, impl_mimeToString( fileId._2 ), thumbnailRecordMaybe flatMap {_.id})

  }



}
//////////////////////////////////////////////////////////////////////////////////
