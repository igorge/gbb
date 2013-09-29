package gie

package collection

import gie.collection.double_linked_list.{DoubleLinkedList, Node}
import scala.collection.mutable
import net.liftweb.common.Loggable

object LRUMapWithNotify {

  trait Updater[V] {
    def remove(value: V): Boolean
    def put(value: V): Unit
    def clear():Unit
    def size:Int
  }

  def makeUpdater[K,V](underlying: scala.collection.mutable.Map[K,V], keyFun: V=>K) = new Updater[V] {
    def remove(value: V): Boolean = underlying.remove( keyFun(value) ).isDefined
    def put(value: V): Unit = underlying.put(keyFun(value), value)
    def clear():Unit = underlying.clear()
    def size:Int = underlying.size
  }

  private class LRUImpl[K,V](
                              maxSize: Int,
                              updater:
                              Updater[V],
                              underlying: mutable.Map[K, Node[(K,V)]] = mutable.HashMap[K, Node[(K,V)]]())
    extends scala.collection.mutable.Map[K,V] with Loggable {

    assume(maxSize>0)

    val usageList = new DoubleLinkedList[(K,V)]()

    type NodeType = Node[(K,V)]

    def iterator: Iterator[(K, V)] = underlying.view.map(v=> v._2.value ).toIterator
    def get(key: K): Option[V] = {
      underlying.get(key).map(_.value._2)
    }

    def -=(key: K) = {

      underlying.remove(key).fold{
        logger.debug(s" Key '${key}' not found in underlying")
      }{node =>
        usageList.unlink(node)
        val isUpdaterRemoved = updater.remove(node.value._2)
        assume(isUpdaterRemoved)
      }

      this
    }

    def +=(kv: (K, V)) = {

      underlying.get(kv._1).fold{
        impl_addNewNode(kv)
      }{node=>
        node.value = kv
        impl_moveToTop(node)
      }

      this
    }

    private def impl_evictIfNeeded(){
      logger.debug(s"impl_evictIfNeeded(${underlying.size}, ${maxSize})")
      if(underlying.size>=maxSize){
        assume(!usageList.isEmpty())
        logger.debug(s"Evicting ${usageList.tail}")

        val evictedNode = usageList.unlink(usageList.tail)
        val removedNodeMaybe = underlying.remove(evictedNode.value._1)
        assume(removedNodeMaybe.isDefined)
        val isUpdaterRemovedNode = updater.remove(evictedNode.value._2)
        assume(isUpdaterRemovedNode)
      }
    }

    private def impl_moveToTop(node: NodeType){
      assume(node ne null)
      usageList.pushFrontNode( usageList.unlink( node ) )
    }


    private def impl_addNewNode(kv: (K, V)){
      impl_evictIfNeeded()

      val node = usageList.pushFront(kv)
      underlying += ((node.value._1, node))
      updater.put(kv._2)
    }

  }


  def apply[K,V](maxSize: Int, updater: Updater[V]): scala.collection.mutable.Map[K,V] =
    new LRUImpl[K,V](maxSize, updater)


}
