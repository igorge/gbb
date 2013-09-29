

package gie.app.gbb

import java.io.{BufferedInputStream, OutputStream, File}
import net.liftweb.common.LazyLoggable
import gie.xadisk._
import scala.util.DynamicVariable
import org.xadisk.bridge.proxies.interfaces.XADiskBasicIOOperations
import org.xadisk.additional.{XAFileInputStreamWrapper, XAFileOutputStreamWrapper}
import net.liftweb.util.Props


object BootFSStorage extends LazyLoggable {
  val TX_PATH = "xadisk.tx_logs_path"
  val STORAGE_PATH = "file_storage"

  val storageRoot = Props.get(STORAGE_PATH).map(new File(_)).getOrElse{
    val errMsg = s"${STORAGE_PATH} was not configured to valid value"
    throw new IllegalArgumentException(errMsg)
  }

  storageRoot.mkdirs()
  assume(storageRoot.isDirectory)

  val dataRoot =  {
    val dr = new File(storageRoot, "data")
    dr.mkdirs()
    dr
  }


  def boot() = Some( ()=>{xaDiskProducer.close()} )

  private val storeMetadata = Props.get(TX_PATH).getOrElse {
    val errMsg = s"${TX_PATH} was not configured to valid value"
    throw new IllegalArgumentException(errMsg)
  }

  val xaDiskProducer = {
    val tmp = new XaDiskResourceProducer(gie.app.gbb.JTA.tm,"XADISK-1", storeMetadata)
    tmp.init()
    tmp
  }

}

object FS extends LazyLoggable {

  def dataRoot = BootFSStorage.dataRoot

  private def buildFileFromId(id: gie.UUID) = new File(dataRoot, id.toString())

  def openFile(id: gie.UUID): Option[()=>java.io.InputStream] = {

    val fs = Coordinator.xaDiskSession()
    val f = buildFileFromId(id)

    if( !fs.fileExists(f) )
      None
    else Some {
      ()=>new BufferedInputStream( new XAFileInputStreamWrapper(fs.createXAFileInputStream(f)) )
    }


  }

  def createFile(): (OutputStream, gie.UUID) = {

    val fs = Coordinator.xaDiskSession()

    val fileIdUUID = gie.UUID()
    val f = buildFileFromId(fileIdUUID)
    fs.createFile(f, false)

    ( new XAFileOutputStreamWrapper(fs.createXAFileOutputStream(f, true)),fileIdUUID)
  }



}
