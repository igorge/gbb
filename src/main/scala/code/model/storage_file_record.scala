package gie.app.gbb.model

import scala.collection.mutable
import gie.app.gbb.{Coordinator, db, DB}
import net.liftweb.common.Loggable
import net.liftweb.util.Props
import gie.collection.LRUMapWithNotify

trait FileRecordResolver {
  def apply(id: Long):Option[db.FileRecord]
  def apply(id: gie.UUID):Option[db.FileRecord]
}


object FileRecord extends Loggable with FileRecordResolver {
  // Record misses are not cached

  val lruCacheSize = Props.getInt("file_record_lru_size").getOrElse(1024)
  logger.info(s"FileRecord LRU cache size is: ${lruCacheSize}")

  private val cacheByUUID = new mutable.HashMap[gie.UUID, db.FileRecord]()
  private val cacheById = LRUMapWithNotify[Long, db.FileRecord](lruCacheSize, LRUMapWithNotify.makeUpdater(cacheByUUID, (v:db.FileRecord)=>v.uuid))


  def withSync[T](fun: =>T):T = synchronized{
    val r = fun
    r
  }

  def apply(id: Long):Option[db.FileRecord] = withSync {
    cacheById.get(id) orElse {
      Coordinator.requireReadOnlyTransaction()
      val record = DB.data.getFileRecordId(id)
      record.foreach{r=>
        logger.debug(s"MISS FileRecord(sizeId=${cacheById.size}, sizeUUID=${cacheByUUID.size}): ${r}")
        cacheById.put(r.id.get, r)
        assume(cacheById.size==cacheByUUID.size)
      }
      record
    }
  }

  def apply(id: gie.UUID):Option[db.FileRecord] = withSync {
    cacheByUUID.get(id) orElse {
      Coordinator.requireReadOnlyTransaction()
      val record = DB.data.getFileRecordByUUID(id)
      record.foreach{r=>
        logger.debug(s"MISS FileRecord(sizeId=${cacheById.size}, sizeUUID=${cacheByUUID.size}): ${r}")
        cacheById.put(r.id.get, r)
        assume(cacheById.size==cacheByUUID.size)
      }
      record
    }
  }

}