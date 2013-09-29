package gie.app.gbb.model.import_export

import gie.app.gbb.{Coordinator, SlickProfile, DB}
import gie.app.gbb.db
import net.liftweb.common.Loggable
import org.joda.time.{DateTimeZone, DateTime}
import java.io.{OutputStreamWriter, FileOutputStream}
import gie.util.{Pipe, file, FileOps}
import gie.app.gbb.FS
import gie.app.gbb.model.{security, time}


object ExportImpl extends Loggable {

  def apply(outRoot: java.io.File){

    security.requirePrivileges(gie.security.privileges.PRIV_ROOT)

    val backupName = "backup " + time.now().toString()
    val outDir = outRoot / backupName
    val filesDir = outDir / FILES
    val boardsFileName = BOARDS_XML
    def mkPostName(id: Long) = s"post-${id}.xml"

    outDir.mkdirs()
    if( !outDir.exists() ) throw new InvalidTargetPathException()
    filesDir.mkdirs()

    Coordinator.loanWrapper(exportTransactionTimeout){
      impl_setupTx()

      val boards = DB.data.boards
      val boardsXml = <boards> { DB.data.boards.map{b=> <board id={b.id.get.toString} path = {b.path} title={b.title} description ={b.description} />} } </boards>
      resource.managed( outDir / boardsFileName |> {new FileOutputStream(_)} |> {new OutputStreamWriter(_, "UTF-8")} ).acquireAndGet{ writer=>
        scala.xml.XML.write(writer, boardsXml, "UTF-8", true, null)
      }


      def postToXml(p: db.Message, fileName: Option[String]) =
        <post id={p.id map (_.toString) getOrElse null}
           childCount={p.childCount.toString}
           name={p.name getOrElse null}
           email={p.email getOrElse null}
           subject={p.subject}
           file={fileName getOrElse null}
           cDate={p.cDate.toString()}
           mDate={p.mDate.toString()}>{p.comment}</post>

      def processPost(p: db.Message, files: Map[Long, db.FileRecord]) = {
        val fileId = for( fileId <- p.file;
             fileRecord <-files.get(fileId);
             uuidString = fileRecord.uuid.toString;
             lazyFileStream <-FS.openFile(fileRecord.uuid)) yield (
                uuidString, resource.managed(lazyFileStream()) and resource.managed( new FileOutputStream(filesDir / uuidString) ) )

        fileId.foreach(_._2.acquireAndGet{ res=>gie.io.Helpers.copy(res._1, res._2, 1*1024*1024) })
        postToXml(p, fileId map {_._1.toString})

      }

      boards.foreach{b=>
        val thisBoardOutPath = outDir / "boards" / b.id.get.toString
        thisBoardOutPath.mkdirs()

        val topicStarters = DB.data.getRootPostsByBoard(b)
        topicStarters.foreach{ currentRootPost=>
          val thisThreadOutDir = thisBoardOutPath / "threads"
          thisThreadOutDir.mkdirs()

          resource.managed( thisThreadOutDir / mkPostName(currentRootPost.id.get) |> {new FileOutputStream(_)} |> {new OutputStreamWriter(_, CHARSET)} ).acquireAndGet{ writer=>
            val fileRecords = DB.data.getFileRecordsForThreadId(currentRootPost)
            val currentThreadPosts = DB.data.getPostsByThreadId(currentRootPost)
            val thread =
              <thread>
                <root> {processPost(currentRootPost, fileRecords)} </root>
                <posts> {currentThreadPosts.map{ processPost(_, fileRecords) }} </posts>
              </thread>
            scala.xml.XML.write(writer, thread, "UTF-8", true, null)
          }

        }

      }

    } // loanWrapper
  }

}










