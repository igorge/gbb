package gie.lift

import net.liftweb.http.SessionVar
import net.liftweb.common.{Full, Empty, Box}

trait SessionMap[T] {
  type A = gie.UUID
  type B = T

  class MapT extends collection.mutable.HashMap[A, B] with collection.mutable.Map[A, B]

  private object IdMap extends SessionVar[MapT]( new MapT() )


  def choice[R](f1: => R)(alternative: => R): R = {
    val isSet = synchronized{ IdMap.set_? }
    if (isSet) f1 else alternative
  }

  def += (kv: (A, B)): this.type = synchronized[this.type]{ IdMap.is+=(kv); this }
  def get(key: A) = synchronized{ IdMap.is.get(key) }

}