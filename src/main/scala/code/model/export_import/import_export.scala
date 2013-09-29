package gie.app.gbb.model

import gie.app.gbb.{SlickProfile, Coordinator}
import gie.util.{Pipe, file, FileOps}

package object import_export {

  val CHARSET =  "UTF-8"
  val FILES = "files"
  val BOARDS_XML = "boards.xml"

  val importTransactionTimeout = 6*60
  val exportTransactionTimeout = 6*60

  class ExportException(message:String = null, cause: Throwable = null) extends Exception(message, cause)

  class ResourceNotFound(message:String = null, cause: Throwable = null) extends ExportException(message, cause)
  class InvalidTargetPathException(message:String = null, cause: Throwable = null) extends ResourceNotFound(message, cause)
  class FileNotFoundException(message:String = null, cause: Throwable = null) extends ResourceNotFound(message, cause)

  class InvalidStructure(message:String = null, cause: Throwable = null) extends ExportException(message, cause)
  class InvalidXML(message:String = null, cause: Throwable = null) extends InvalidStructure(message, cause)
  class InvalidBoardRecord(message:String = null, cause: Throwable = null) extends InvalidStructure(message, cause)
  class InvalidBackupData(message:String = null, cause: Throwable = null) extends InvalidStructure(message, cause)

  def export(outRoot: String):Unit = this.export( file(outRoot) )
  def export(outRoot: java.io.File):Unit = ExportImpl(outRoot)

  def import_(inRoot: String):Unit = this.import_( file(inRoot) )
  def import_(inRoot: java.io.File):Unit = ImportImpl(inRoot)


  private[import_export] def impl_setupTx(){
    import gie.app.gbb.DB.slickSession

    SlickProfile.utils.setSerializableForTransaction()
  }
}
