package gie.app.gbb.model
//////////////////////////////////////////////////////////////////////////////////

import gie.app.gbb._
import net.liftweb.http._
import net.liftweb.common.{Box, Empty, LazyLoggable}
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import net.liftweb.http.rest.RestHelper
import java.io.{BufferedInputStream, InputStream}
import scala.collection.mutable.Builder
import scala.xml.{NodeSeq, Node}

object Board extends LazyLoggable { self=>

  val childPostsLimit = 3

  val BOARD_ID = "boardId"
  val POST_ID = "postId"
  val INVALID_BOARD_MSG = "Invalid board"
  val INVALID_POST_MSG = "Invalid post"
  val OBJECT_SIZE_TO_BIG_MSG = "Too big"

  object abort {
    def invalidBoardId(id: Option[Long]): Nothing = {
      logger.error("Invalid board id :" + id)
      Coordinator.doRollbackOnRequestEnd()
      throw new ResponseShortcutException(()=>new NotFoundResponse(INVALID_BOARD_MSG), Empty, false)
    }
    def invalidPostId(id: String): Nothing = {
      logger.error("Invalid post id :" + id)
      Coordinator.doRollbackOnRequestEnd()
      throw new ResponseShortcutException(()=>new NotFoundResponse(INVALID_POST_MSG), Empty, false)
    }
    def invalidPostId(id: Option[Long]): Nothing = {
      logger.error("Invalid post id :" + id)
      Coordinator.doRollbackOnRequestEnd()
      throw new ResponseShortcutException(()=>new NotFoundResponse(INVALID_POST_MSG), Empty, false)
    }
    def invalidFormComponentSize(s: Int): Nothing = {
      logger.error("Invalid form component size :" + s)
      Coordinator.doRollbackOnRequestEnd()
      throw new ResponseShortcutException(()=>new NotFoundResponse(OBJECT_SIZE_TO_BIG_MSG), Empty, false)
    }
    def rootPostCannotBeAPartOfThread(rootPostId: Long): Nothing = {
      logger.error(s"Root post id='${rootPostId}' cannot be a part of thread")
      Coordinator.doRollbackOnRequestEnd()
      throw new ResponseShortcutException(()=>new NotFoundResponse(INVALID_POST_MSG), Empty, false)
    }
  }

  def boot() = {
//    boards.foreach { b=>
//      val id = b.id.get.toString
//      logger.debug(s"registering board url rewrite ${b.path} to /board/${id}")
//      LiftRules.statelessRewrite.prepend {
//        case RewriteRequest(ParsePath("board" :: b.path :: Nil, _, _, _), _, _ ) =>
//          RewriteResponse("board" :: Nil, Map(BOARD_ID -> id))
//      }
//      LiftRules.statelessRewrite.prepend {
//        case RewriteRequest(ParsePath("board" :: b.path :: "msg" :: postId :: Nil, _, _, _), _, _ ) =>
//          RewriteResponse("board" :: Nil, Map(BOARD_ID -> id, POST_ID -> postId))
//      }
//    }

    LiftRules.statefulRewrite.prepend {

      case RewriteRequest(ParsePath("board" :: boardName :: "msg" :: postId :: Nil, _, _, _), _, _ ) =>
        val boardMaybe = boards.find(_.path==boardName)
        boardMaybe.fold{
          logger.debug(s"Board '${boardName}' have not been found")
          RewriteResponse("board" :: Nil, Map(BOARD_ID -> "invalid_board_name"))
        }{
          board=> RewriteResponse("board" :: Nil, Map(BOARD_ID ->  board.id.get.toString, POST_ID -> postId))
        }

      case RewriteRequest(ParsePath("board" :: boardName :: Nil, _, _, _), _, _ )=>
        val boardMaybe = boards.find(_.path==boardName)
        boardMaybe.fold[RewriteResponse]{
          logger.debug(s"Board '${boardName}' have not been found")
          RewriteResponse("board" :: Nil, Map(BOARD_ID -> "invalid_board_name"))
        }{
          board=> RewriteResponse("board" :: Nil, Map(BOARD_ID -> board.id.get.toString))
        }

    }

    DB.event.boardsUpdated.subscribe(impl_boardsUpdated)

    None
  }

  private var cachedBoardList:Option[Array[db.Board]] = None
  private object impl_boardsUpdated extends DBChangeListener {
    def update() = self.synchronized{
      logger.debug("Boards underlying data have changed, resetting cache")
      cachedBoardList = None
    }
  }

  def boardIdToPath(id: Long): Option[String] = {
    boards.find(_.id.get == id).map(_.path)
  }


  def urlPath(b: db.Board) = "/board/"+b.path
  def urlForPost(msg: db.Message) = s"/board/${ boardIdToPath(msg.boardId).get }/msg/${msg.id.get}#post${msg.id.get}"
  def urlForBoardWithPost(board: db.Message, post: db.Message) = s"${board.id.get}#post${post.id.get}"

  def urlForThumbnail(f: db.FileRecord) = urlForFile(f)
  def urlForFile(f: db.FileRecord) = s"/img/${f.uuid.toString}"

  def processComment(msg: String): xml.NodeSeq = {
    gie.raw2xml.parse(msg)
  }

  def boards: Seq[db.Board] = self.synchronized {
    if(cachedBoardList.isEmpty){
      logger.debug("Feeding boards cache")
      cachedBoardList=Some(DB.data.boards)
    } else {
      //logger.debug("Returning cached boards")
    }

    cachedBoardList.get
  }

  def isBoardValid_?(id: Box[Long]): Boolean = {
    id flatMap { getBoardById(_) } map {_ => true} getOrElse { false }
  }

  def getPostById(id: Long ) = DB.data.getPostById(id)

  def getBoardById(id: Long) = {
    boards.find(_.id.get == id)
  }

  def getRootPostsByBoard(boardId: Long) = DB.data.getRootSortedPostsByBoard(boardId)
  def getPostsByThreadId(threadId: Long) = DB.data.getSortedPostsByThreadId(threadId)
  def getPostsByThreadId(threadId: Long, limit: Int) = DB.data.getSortedPostsByThreadId(threadId, limit)
  //def getFileRecordsForThreadId(threadId: Long, limit: Int = 0)  = DB.data.getFileRecordsForThreadId(threadId, limit)
  //def getFileRecordsForBoardRootPosts(boardId: Long) = DB.data.getFileRecordsForBoardRootPosts(boardId)

  def getRootPostsByBoardWithChildren(boardId: Long): Array[(db.Message, Array[db.Message])] = {
    getRootPostsByBoard(boardId) map {rootPost=>(rootPost,getPostsByThreadId(rootPost.id.get, childPostsLimit))}
  }

  def postNewTopicEx[T](msg: db.Message, lazyMaybeFile: Option[()=>InputStream])(fun: db.Message=>T) = {
    val fileRecordMaybe = lazyMaybeFile map { lazyFile=>
      resource.managed( lazyFile() ).acquireAndGet{ FileStore.store( _ ) }
    }

    val msg1 = msg.copy(file = fileRecordMaybe flatMap {_.id})
    fun(msg1)
  }

  def postNewTopic(msg: db.Message, file: Option[FileParamHolder]) = {
    assume(msg.id.isEmpty)
    assume(msg.threadId.isEmpty)

    postNewTopicEx(msg, file map {holder => ()=>holder.fileStream }){ msg=>
      val updatedMsg = DB.data.insertNewPost(msg)
      logger.debug("INSERTED NEW MESSAGE: "+updatedMsg)
      updatedMsg
    }
  }


  def postReply(rootMsg: db.Message, msg: db.Message, file: Option[FileParamHolder], updateRootMsgTimeStamps:Boolean = true) = {
    assume(rootMsg.id.isDefined)
    assume(msg.id.isEmpty)
    assume(msg.threadId.isEmpty)

    postNewTopicEx(msg.copy(threadId = rootMsg.id), file map {holder => ()=>holder.fileStream }){ msg=>
      val updatedMsg = DB.data.postReply(rootMsg, msg, updateRootMsgTimeStamps)
      logger.debug(s"INSERTED REPLY MESSAGE: ${updatedMsg} for ${rootMsg}")
      updatedMsg
    }
  }

}

