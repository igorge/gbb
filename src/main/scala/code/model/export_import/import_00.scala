package gie.app.gbb.model.import_export

import gie.app.gbb.model.{security, time, Board, FileStore}
import gie.app.gbb.FS
import gie.util.{Pipe, file, FileOps}
import gie.app.gbb.{Coordinator, DB, db}
import java.io.{BufferedInputStream, FileInputStream, InputStream, InputStreamReader}
import net.liftweb.common.Loggable
import scala.reflect.{classTag}
import org.joda.time.DateTime
import net.liftweb.http.FileParamHolder


object ImportImpl00 extends Loggable {

  val X_BOARDS = "boards"
  val X_BOARD = "board"
  val X_ID = "id"
  val X_PATH = "path"
  val X_TITLE = "title"
  val X_DESCRIPTION = "description"
  val X_THREAD = "thread"
  val X_THREADS = "threads"
  val X_ROOT = "root"
  val X_POST = "post"
  val X_POSTS = "posts"
  val X_CHILDCOUNT = "childCount"
  val X_SUBJECT = "subject"
  val X_EMAIL = "email"
  val X_FILE = "file"
  val X_MDATE = "mDate"
  val X_CDATE = "cDate"
  val X_FILES = "files"


  private def impl_try[T](r: => T): scala.util.Try[T] = gie.Try.nonfatal(classTag[ExportException])(r)

  private def impl_getAttr(node: xml.NodeSeq, attrName: String):String = {
    val attrNode = node(0).attribute(attrName).getOrElse{throw new InvalidXML(s"attr '${attrName}' not found")}
    val count = attrNode.length
    if(count>1) throw new InvalidXML(s"to many attrs of name '${attrName}'")
    if(count==0) throw new InvalidXML(s"value of attr '${attrName}' is not set")
    attrNode(0).text
  }

  private def impl_getLongAttr(node: xml.NodeSeq, attrName: String): Long = {
    val attrStringVal = impl_getAttr(node, attrName)
    try attrStringVal.toLong catch { case e:Throwable => throw new InvalidXML(e.getMessage(), e) }
  }

  private def impl_getAttrMaybe(node: xml.NodeSeq, attrName: String):  scala.util.Try[String] = impl_try{ impl_getAttr(node, attrName) }
  private def impl_getLongAttrMaybe(node: xml.NodeSeq, attrName: String):  scala.util.Try[Long] = impl_try{ impl_getLongAttr(node, attrName) }


  private def impl_maybeGetSingleChild(node: xml.Node, name: String):Option[xml.Node] = {
    val n = node \ name
    if(n.size > 1) throw new InvalidXML(s"require 1 child of <${name}> tag, but found ${n.size}")
    if(n.size==0) None else Some(n(0))
  }

  private def impl_getSingleChild(node: xml.Node, name: String):xml.Node =
    impl_maybeGetSingleChild(node, name).fold[xml.Node]{throw new InvalidXML(s"require <${name}> tag, but no node found. Scope: ${node}")}{ n => n }

  private def impl_checkXmlNodeType(n: xml.Node, requireType: String){
    if(n eq null) throw new InvalidXML(s"require <${requireType}> tag, but no node found")
    if( n.label!=requireType ) throw new InvalidXML(s"require <${requireType}> tag, but found <${n.label}>")
  }

  private def impl_readBoards(ir: InputStreamReader)(fun: db.Board=>Unit) = {
    val boardsXml = xml.XML.load(ir)
    impl_checkXmlNodeType(boardsXml, X_BOARDS)

    val boards = boardsXml \ X_BOARD

    boards.foreach{i=>
      val id = impl_getLongAttr(i, X_ID)
      val path = impl_getAttr(i, X_PATH)
      val title = impl_try{ impl_getAttr(i, X_TITLE) } getOrElse ""
      val description = impl_try{ impl_getAttr(i, X_DESCRIPTION) } getOrElse ""
      fun( db.Board( Some(id), title, description, path) )
    }

  }

  private class ImplProcessBoard(val inRoot: java.io.File) extends (db.Board=>Unit){
    private val impl_presentBoards = DB.data.boards

    logger.debug(s"Found boards: ${impl_presentBoards.toSeq}")

    private def impl_create(board: db.Board){
      logger.debug(s"No board '${board}' present in DB")
      val dbBoard = board.copy( id = Some(DB.data.insertBoard( board.copy(id=None) ) ) )
      logger.debug(s"Created board '${board}' as '${dbBoard}'")
      impl_withDbBoard(board, dbBoard)
    }

    private def impl_withDbBoard(board: db.Board, dbBoard: db.Board){
      logger.debug(s"Board '${board}' already present in DB as '${dbBoard}'")
      val onDiskBoardId = board.id.map{ _.toString } getOrElse {throw new InvalidBoardRecord(board.toString)}
      val onDiskPath = inRoot / X_BOARDS / onDiskBoardId / X_THREADS
      if( !onDiskPath.exists() ) throw new InvalidBackupData(s"'${onDiskPath}' not found")
      val openTopicFile = impl_openTopicFile(dbBoard)_
      onDiskPath.listFiles() foreach (openTopicFile)
    }

    private def impl_openTopicFile(board: db.Board)(topicFile: java.io.File){
      resource.managed{ topicFile.open_!() |> {new InputStreamReader(_, CHARSET)} }.acquireAndGet{ ir=>
        val possiblyTopicXml = xml.XML.load(ir)
        impl_checkXmlNodeType(possiblyTopicXml, X_THREAD)
        impl_loadThread(board)( possiblyTopicXml )
      }
    }

    private def impl_loadThread(board: db.Board)(parent: xml.Node){
      val rootPost = impl_getSingleChild(parent, X_ROOT) |> {impl_getSingleChild(_, X_POST)} |> impl_loadPost
      val posts = (impl_getSingleChild(parent,X_POSTS) \ X_POST).view map impl_loadPost

      impl_processThread(board)(rootPost, posts)
    }

    private def impl_isSpaceChar(c: Char):Boolean = c == ' ' || c == '\n' || c == '\r' || c == '\t'
    private def impl_stringFilterLift(s: String) = if(s.find{ !impl_isSpaceChar(_) }.isEmpty) {
      None
    } else {
      Some(s)
    }

    type RawPost = (db.Message, scala.util.Try[gie.UUID])
    private def impl_loadPost(node: xml.Node): RawPost = {
      impl_checkXmlNodeType(node, X_POST)
      if( node.child.size>1 ) throw new InvalidXML(s"<${X_POST}> node must not have any child") // 1 child for text PCDATA

      object post {
        val id = impl_getLongAttrMaybe(node, X_ID)
        val childCount = impl_getLongAttrMaybe(node, X_CHILDCOUNT)
        val subject = impl_getAttrMaybe(node, X_SUBJECT)
        val email = impl_getAttrMaybe(node, X_EMAIL)
        val file = impl_getAttrMaybe(node, X_FILE) map {gie.UUID(_) getOrElse (throw new InvalidXML("UUID parsing have failed"))}
        val cDate = impl_getAttrMaybe(node, X_CDATE) map {DateTime.parse(_)}
        val mDate = impl_getAttrMaybe(node, X_MDATE) map {DateTime.parse(_)}
        val text = impl_stringFilterLift( node.text )
      }

      val p = db.Message(
        id = post.id.toOption,
        boardId = -1,
        threadId = None,
        childCount = 0,
        name = None,
        email = post.email.toOption,
        subject = post.subject getOrElse (""),
        comment = post.text getOrElse (""),
        file = None,
        cDate = post.cDate getOrElse( time.now() ),
        mDate = post.mDate getOrElse( time.now() )
      )

      (p, post.file)
    }

    private def impl_lazyOpenFileMaybe[T](post: RawPost): Option[FileParamHolder] = {
      val lazyFileMaybe = post._2 map { fileUUID => ()=>( inRoot / X_FILES / fileUUID.toString).open_!() |> {new BufferedInputStream(_)} }

      lazyFileMaybe map {lazyIs=>
        new FileParamHolder("","","") {
          def fileStream: InputStream = lazyIs()
          def file: Array[Byte] = ???
          def length: Long = ???
        }
      } toOption
    }

    private def impl_processThread(board: db.Board)(rootPost: RawPost, posts: Seq[RawPost]){
      val topicStarter = rootPost._1.copy(id = None, boardId = board.id.get)
      val updatedRootPost = Board.postNewTopic( topicStarter, impl_lazyOpenFileMaybe(rootPost) )
      val threadRepliesProcessor = impl_processThreadReply(board, updatedRootPost)_
      posts.foreach{threadRepliesProcessor}
    }

    private def impl_processThreadReply(board: db.Board, rootPost: db.Message)(post: RawPost){
      val reply = post._1.copy(id=None, boardId = board.id.get)
      val updatedRootPost = Board.postReply( rootPost, reply, impl_lazyOpenFileMaybe(post), false )
    }



    def apply(board: db.Board){
      assume(!board.path.isEmpty)
      impl_presentBoards.find{ _.path==board.path }.fold{impl_create(board)}{impl_withDbBoard(board,_)}
    }
  }

  def apply(inRoot_ : java.io.File):Unit = try {
    Coordinator.loanWrapper(importTransactionTimeout){

      impl_setupTx()

      val inRoot = file("/tmp/temp/backup 2013-09-08T23:40:07.087+03:00") //DEBUG

      security.requirePrivileges(gie.security.privileges.PRIV_ROOT)
      if(!inRoot.exists()) throw new InvalidTargetPathException(s"'${inRoot.toString}' not found")
      val boardsXmlPath = inRoot / BOARDS_XML

      val boards = resource.managed( boardsXmlPath.open_! |> {new InputStreamReader(_, CHARSET)} ).acquireAndGet{ir=> impl_readBoards(ir)(new ImplProcessBoard(inRoot)) }



    }

    DB.event.boardsUpdated()

  } catch {
    case e:java.io.FileNotFoundException => throw new FileNotFoundException(e.getMessage, e)
  }


}

