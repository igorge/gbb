package gie.app.gbb

import gie.app.gbb.db.{Message, MessagesTable}
import gie.UUID
import SlickProfile.profile.simple._
import scala.slick.jdbc.meta.{MTable}
import scala.collection.mutable.ArrayBuffer
import scala.ref.WeakReference
import scala.collection.mutable
import scala.slick.lifted

//import org.h2.jdbcx.JdbcConnectionPool
import scala.slick.session.Database
import net.liftweb.common.Loggable
import net.liftweb.util.Props
import com.github.nscala_time.time.Imports._
import gie.DateTimeMapper

//////////////////////////////////////////////////////////////////////////////////
object StorageConstants {
  val KEY_DB_PATH:String = "db.path"
}

trait StorageTrait { this: Loggable =>
  protected val dbHandle:Database
  protected def openDb(): Database


  implicit def slickSession:Session

  protected def collectMissingTables[T <: Table[_]](availTables:Map[String, MTable])(tables:T*): Option[scala.slick.lifted.DDL] = {
    tables.foldLeft[Option[scala.slick.lifted.DDL]](None){(acum,t)=>
      if(availTables.contains(t.tableName)) acum else Some(acum.fold( t.ddl ){_  ++ t.ddl})
    }
  }

  protected def makeTableMap(implicit dbsess: Session) : Map[String, MTable] = {
    val tableList = MTable.getTables.list()(dbsess)
    val tableMap = tableList.map{t => (t.name.name, t)}.toMap
    logger.debug(s"Found tables: ${tableMap.map(_._1)}")
    tableMap
  }

  protected def createIfNotExists[T](tables:Map[String, MTable], table:Table[T])(implicit dbsess: Session){
    if(!tables.contains(table.tableName)) {
      //println(s"DEBUG: DDL CREATE STM: ${table.ddl.createStatements.mkString}")
      table.ddl.create(dbsess)
    }
  }
  protected def createIfNotExists[T](tables:Map[String, MTable])(tablesToCreate:Table[T]*)(implicit dbsess: Session){
    tablesToCreate.foreach( createIfNotExists(tables, _)(dbsess) )
  }

}

object DBBoot {
  val h2DBLockTimeOut = 10000 //10 sec

  private val url1 = s"jdbc:h2:${Props.get(StorageConstants.KEY_DB_PATH).openOrThrowException("DB path key not found") };AUTO_SERVER=TRUE;TRACE_LEVEL_FILE=3;LOCK_TIMEOUT=${h2DBLockTimeOut}"
//  private val url2 = s"jdbc:h2:${Props.get(StorageConstants.KEY_DB_PATH).openOrThrowException("DB path key not found") };AUTO_SERVER=TRUE;TRACE_LEVEL_SYSTEM_OUT=1"
//  private val url3 = s"jdbc:h2:${Props.get(StorageConstants.KEY_DB_PATH).openOrThrowException("DB path key not found") };AUTO_SERVER=TRUE"

  def connetionUrl: String = url1

  private class StorageBooter extends Loggable with StorageTrait {

    protected val dbHandle:Database = openDb()
    implicit def slickSession:Session = Database.threadLocalSession


    protected def openDb() = {
      //dbPool = JdbcConnectionPool.create(s"jdbc:h2:${Props.get(Storage.KEY_DB_PATH).openOrThrowException("DB path key not found") };AUTO_SERVER=TRUE;TRACE_LEVEL_FILE=2","","")
      Database.forURL(connetionUrl)
    }

    private def boot_insertBoards() = {
//      if( Query(db.BoardsTable.length).first==0 ){
//        logger.debug("Booting boards")
//
//        {
//          val bb = db.Board(None, "Anime", "anime", "a")
//          val id = db.BoardsTable.*.returning(db.BoardsTable.id).insert(bb)
//        }
//        {
//          val bb = db.Board(None, "Random", "Random", "b")
//          val id = db.BoardsTable.*.returning(db.BoardsTable.id).insert(bb)
//        }
//        {
//          val bb = db.Board(None, "Technology", "Technology", "g")
//          val id = db.BoardsTable.*.returning(db.BoardsTable.id).insert(bb)
//        }
//      }
    }

    private def bootDDL() = {

      logger.info("DDL boot...")
      val availTables = makeTableMap

      val ddlMaybe = collectMissingTables(availTables)(db.BoardsTable, db.FileRecordsTable, db.MessagesTable)

      ddlMaybe.foreach(_.create)
    }


    def boot() = dbHandle.withTransaction {
      logger.debug("StorageBoot ...")
      bootDDL()
      boot_insertBoards()

      logger.debug("StorageBoot done")
      None
    }


  }

  def boot() = { new StorageBooter().boot() }

}

trait DBChangeListener {
  def update():Unit
}

trait DBChangePublisher { self=>
  private var listeners = new Array[WeakReference[DBChangeListener]](0)
  private var waste = 0
  private val wasteTh = 8
  private def impl_doGC() = if(waste>wasteTh) {
    listeners = listeners.filter{_.get.fold(true){ _ => false } }
    waste = 0
  }
  def subscribe(listener: DBChangeListener) = self.synchronized {
    listeners = listeners :+ WeakReference(listener)
  }
  def unsubscribe(listener: DBChangeListener)  = self.synchronized {
    listeners = listeners.filter{wr=> wr.get.fold(false){ _ eq listener } }
  }

  def changed() = self.synchronized{
    impl_doGC()
    listeners.foreach{ _.get.fold{ waste+=1 }{ _.update() } }
  }

  def apply() = changed()
}

object DB extends Loggable with StorageTrait {

  implicit def slickSession:Session = Coordinator.slickSession

  protected def openDb() = {
    assume(JTA.mainDBSource() ne null)
    Database.forDataSource(JTA.mainDBSource)
  }

  val dbHandle = openDb()

  def createSession() = dbHandle.createSession()

  //store layer
  object event {
    object boardsUpdated extends DBChangePublisher
  }


  object data {

    def boards: Array[db.Board] = {
      (for (v <- db.BoardsTable) yield v).to[Array]
    }

    def isBoardValid_?(id: Long): Boolean = {

      val q = (for( v <- db.BoardsTable if v.id === id) yield v.id.count )

      (q.first != 0)
    }

    def insertBoard(board: db.Board) = {
      db.BoardsTable.*.returning(db.BoardsTable.id).insert(board)
    }

    def postReply(rootMsg: db.Message, msg: db.Message, updateRootMsgTimeStamps:Boolean = true) = {
      import scala.slick.jdbc.{GetResult, StaticQuery => Q}

      val id = db.MessagesTable.*.returning(db.MessagesTable.id).insert(msg)

//      val q = for( t<-db.MessagesTable if t.id === rootMsg.id ) yield t.mDate
//      q.update( DateTime.now )
      if(updateRootMsgTimeStamps){
        val updateSetPart = s"UPDATE ${'"'}${db.MessagesTable.tableName}${'"'} SET"
        val wherePart = s"WHERE ${'"'}${db.MessagesTable.C_ID}${'"'} = ?"
        val directQ = Q.update[(java.sql.Timestamp, Long) ](s"${updateSetPart} ${'"'}${db.MessagesTable.C_MODIFICATION_DATE}${'"'} = ? , ${'"'}${db.MessagesTable.C_CHILD_COUNT}${'"'} = ${'"'}${db.MessagesTable.C_CHILD_COUNT}${'"'} + 1 ${wherePart};")
        directQ.execute( DateTimeMapper.map(DateTime.now), rootMsg.id.get)
      } else {
        val updateSetPart = s"UPDATE ${'"'}${db.MessagesTable.tableName}${'"'} SET"
        val wherePart = s"WHERE ${'"'}${db.MessagesTable.C_ID}${'"'} = ?"
        val directQ = Q.update[Long](s"${updateSetPart} ${'"'}${db.MessagesTable.C_CHILD_COUNT}${'"'} = ${'"'}${db.MessagesTable.C_CHILD_COUNT}${'"'} + 1 ${wherePart};")
        directQ.execute(rootMsg.id.get)
      }

      msg.copy(id = Some(id) )
    }

    def insertNewFileRecord(msg: db.FileRecord) =  {
      val id = db.FileRecordsTable.*.returning(db.FileRecordsTable.id).insert(msg)
      val updated = msg.copy(id = Some(id) )
      logger.debug(s"INSERTED FileRecord: ${updated}")
      updated
    }

    def getFileRecordByUUID(id: UUID): Option[db.FileRecord] = {
      import gie.UUIDMapper._

      val q = for(
        //fileId <-Parameters[UUID];
        t <- db.FileRecordsTable if t.uuid === id.bind
      ) yield t

      q.firstOption
    }

    def getFileRecordId(id: Long): Option[db.FileRecord] = {
      import gie.UUIDMapper._
      val q = for(t <- db.FileRecordsTable if t.id === id ) yield t
      q.firstOption
    }

    def getFileRecordAndThumbNail(id: Long): Option[(db.FileRecord, Option[db.FileRecord])] = {
      val mainRecord = (for(t1 <- db.FileRecordsTable if t1.id === id) yield t1).firstOption

      val thumbnail = mainRecord flatMap {main =>
        (for(t <- db.FileRecordsTable if t.id === main.thumbnailId ) yield t).firstOption
      }

      mainRecord map { main =>
        (main, thumbnail)
      }

    }

    def insertNewPost(msg: db.Message) =  {
      val id = db.MessagesTable.*.returning(db.MessagesTable.id).insert(msg)
      msg.copy(id = Some(id) )
    }

    def getBoardById(id: Long) =  {
      (for(t <- db.BoardsTable if t.id === id ) yield t).firstOption
    }

    private def query_getPostById(id: Long)  = (for(t <- db.MessagesTable if t.id === id ) yield t)
    def getPostById(id: Long) =query_getPostById(id).firstOption

    private def query_getRootPostsByBoard(boardId: Long) = (for( t <- db.MessagesTable if (t.boardId === boardId) && (t.threadId.isNull) ) yield t)
    def getRootPostsByBoard(board: db.Board) = query_getRootPostsByBoard(board.id.get).to[Array]
    def getRootSortedPostsByBoard(boardId: Long) =  {
      val q  = query_getRootPostsByBoard(boardId).sortBy( _.mDate.desc )
      q.to[Array]
    }

    private def query_getPostsByThreadId(threadId: Long)  = (for( t <- db.MessagesTable if (t.threadId === threadId) ) yield t)
    private def query_getSortedPostsByThreadId(threadId: Long) = query_getPostsByThreadId(threadId).sortBy( _.cDate.asc )

    def getPostsByThreadId(m: db.Message) = query_getPostsByThreadId(m.id.get).to[Array]
    def getSortedPostsByThreadId(threadId: Long) =  {
      val q  = query_getSortedPostsByThreadId(threadId)
      q.to[Array]
    }

    def getSortedPostsByThreadId(threadId: Long, limit: Int) =  {
      assume(limit>0)
      val q = query_getPostsByThreadId(threadId).sortBy( _.cDate.desc ).take(limit)
      q.to[Array].reverse
    }

    //lifted.Query[MessagesTable.type, Message]


    def getFileRecordsForBoardRootPosts(boardId: Long): Map[Long, db.FileRecord] =  {
      val postsQ = query_getRootPostsByBoard(boardId)

      val mainRecords = for( t <- postsQ;
                             f <- db.FileRecordsTable if f.id === t.file) yield f

      val thumbnailRecords = for( f  <- mainRecords;
                                  f2 <- db.FileRecordsTable if f2.id === f.thumbnailId) yield f2

      (mainRecords union thumbnailRecords).foldLeft(Map[Long, db.FileRecord]()){ (accum, record) =>  accum + (record.id.get -> record) }
    }

    def query_getFileRecordsForThreadId(threadId: Long, childLimit: Int = 0) = {
      val q_thisPost = query_getPostById(threadId)
      val q_childPosts = if(childLimit>0) query_getPostsByThreadId(threadId).sortBy( _.cDate.desc ).take(childLimit) else query_getPostsByThreadId(threadId)

      val postRecordQ = for(post <- q_thisPost;
                               t <- db.FileRecordsTable if t.id === post.file) yield t

      val postRecordThumbnailQ = for(t  <- postRecordQ;
                                     t2 <- db.FileRecordsTable if t2.id===t.thumbnailId) yield t2


      val mainRecords = for( t <- q_childPosts;
                             f <- db.FileRecordsTable if f.id === t.file) yield f

      val thumbnailRecords = for( f  <- mainRecords;
                                  f2 <- db.FileRecordsTable if f2.id === f.thumbnailId) yield f2

      (postRecordQ union postRecordThumbnailQ union mainRecords union thumbnailRecords)
    }

    def getFileRecordsForThreadId(thread: db.Message):Map[Long, db.FileRecord] = getFileRecordsForThreadId(thread.id.get)
    def getFileRecordsForThreadId(threadId: Long, childLimit: Int = 0): Map[Long, db.FileRecord] =  {
      query_getFileRecordsForThreadId(threadId).foldLeft(Map[Long, db.FileRecord]()){ (accum, record) =>  accum + (record.id.get -> record) }
    }

  }

}