package gie

import scala.slick.lifted.{TypeMapper, MappedTypeMapper}

object UUIDMapper {

  implicit val  uuidMap = MappedTypeMapper.base[UUID, java.util.UUID] (
    uuid => uuid.jUuid,
    uuid => new UUID(uuid)
  )

}
