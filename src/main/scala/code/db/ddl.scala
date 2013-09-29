package gie.app.gbb.db
//////////////////////////////////////////////////////////////////////////////////

import gie.app.gbb.SlickProfile.profile.simple._

import com.github.nscala_time.time.Imports._
import gie.UUIDMapper._
import gie.DateTimeMapper._

//////////////////////////////////////////////////////////////////////////////////
case class Board(id:Option[Long],
  title: String,
  description: String,
  path: String )

object BoardsTable extends Table[Board]("boards"){
  def id = column[Long]("id", O.DBType("IDENTITY"), O.PrimaryKey, O.AutoInc)
  def title = column[String]("title")
  def description = column[String]("description")
  def path = column[String]("path")

  def * = id.? ~ title ~ description ~ path<> (Board, Board.unapply _)
  def forInsert = title ~ description ~ path <> ({v=>Board(None,v._1, v._2, v._3)}, {v:Board=>Some(v.title, v.description, v.path)})
}

//////////////////////////////////////////////////////////////////////////////////
case class Message(id: Option[Long] = None,
  boardId: Long,
  threadId: Option[Long] = None,
  childCount: Int = 0,
  name: Option[String] = None,
  email: Option[String] = None,
  subject: String,
  comment: String,
  file: Option[Long] = None,
  cDate: DateTime = DateTime.now,
  mDate: DateTime = DateTime.now)

object MessagesTable extends Table[Message]("messages"){
  val C_MODIFICATION_DATE = "modification_date"
  val C_CHILD_COUNT = "child_count"
  val C_ID = "id"

  def id = column[Long](C_ID, O.DBType("IDENTITY"), O.PrimaryKey, O.AutoInc)
  def boardId = column[Long]("board_id")
  def threadId = column[Long]("thread_id", O.Nullable)
  def childCount = column[Int](C_CHILD_COUNT, O.Default(0))
  def name = column[String]("name", O.Nullable)
  def email = column[String]("email", O.Nullable)
  def subject = column[String]("subject")
  def comment = column[String]("message")
  def file = column[Long]("file_id", O.Nullable)
  def cDate = column[DateTime]("creation_date")
  def mDate = column[DateTime](C_MODIFICATION_DATE)

  def cDateIdx = index("messages_cDate_idx", cDate)
  def mDateIdx = index("messages_mDate_idx", mDate)
  def topicFK = foreignKey("messages_threadId_fk", threadId, MessagesTable)(_.id)
  def boardFK = foreignKey("messages_boardId_fk", boardId, BoardsTable)(_.id)
  def fileFK = foreignKey("file_id_fk", file, FileRecordsTable)(_.id)

  def * = id.? ~ boardId ~ threadId.? ~ childCount ~ name.? ~ email.? ~ subject ~ comment ~ file.? ~ cDate ~ mDate  <> (Message, Message.unapply _)

}
//////////////////////////////////////////////////////////////////////////////////
case class FileRecord(id: Option[Long] = None,
  uuid: gie.UUID,
  mime: String,
  thumbnailId: Option[Long] = None)

object FileRecordsTable extends Table[FileRecord]("file_records"){
  def id = column[Long]("id", O.DBType("IDENTITY"), O.PrimaryKey, O.AutoInc)
  def uuid = column[gie.UUID]("file_uuid")//(UUIDMapper.uuidMap)
  def mime = column[String]("mime")
  def thumbnailId = column[Long]("thumbnail_id", O.Nullable)

  def thumbnailFK = foreignKey(s"${this.tableName}_thumbnailId_fk", thumbnailId, FileRecordsTable)(_.id)
  def uuidIdx = index(s"${this.tableName}_uuid_fk", uuid, unique = true)

  def * = id.? ~ uuid ~ mime ~ thumbnailId.? <> (FileRecord, FileRecord.unapply _)
}

