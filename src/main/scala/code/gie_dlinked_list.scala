// (c) GIE: Simple double link list


package gie.collection

package double_linked_list

trait DoubleLinkedListException {}

class DoubleLinkedList_NoSuchElementException() extends NoSuchElementException() with DoubleLinkedListException
class DoubleLinkedList_IllegalArgumentException() extends IllegalArgumentException() with DoubleLinkedListException

trait Node[T] {
  var prevNode:Node[T]
  var nextNode:Node[T]
  var value:T
}

final class DefaultNode[T] extends Node[T] {
  var prevNode:Node[T] = _
  var nextNode:Node[T] = _
  var value:T = _
}

final class DoubleLinkedList[T](private val factoryFun: ()=>Node[T] = ()=> new DefaultNode[T]) {
  type NodeType = Node[T]

  private var impl_head: NodeType = null
  private var impl_tail: NodeType = null

  def newNode() = factoryFun()
  def newNode(value: T): NodeType = {
    val newNode = factoryFun()
    newNode.value = value
    newNode
  }

  def isEmpty():Boolean = {
    if(impl_head eq null) {
      assume(impl_tail eq null)
      true
    } else {
      assume(impl_tail ne null)
      false
    }
  }

  def head: NodeType = {
    if(impl_head eq null)  throw new DoubleLinkedList_NoSuchElementException()
    impl_head
  }
  def tail: NodeType = {
    if(impl_tail eq null)  throw new DoubleLinkedList_NoSuchElementException()
    impl_tail
  }

  def nodeIterator: Iterator[NodeType] = new Iterator[NodeType]{
    var currentNode = impl_head
    def hasNext: Boolean = currentNode ne null
    def next(): NodeType = {
      if(!hasNext) throw new DoubleLinkedList_NoSuchElementException()
      val n = currentNode
      currentNode = n.nextNode
      n
    }
  }

  def iterator: Iterator[T] = new Iterator[T]{
    var currentNode = impl_head
    def hasNext: Boolean = currentNode ne null
    def next(): T = {
      if(!hasNext) throw new DoubleLinkedList_NoSuchElementException()
      val n = currentNode
      currentNode = n.nextNode
      n.value
    }
  }

  def reverseNodeIterator: Iterator[NodeType] = new Iterator[NodeType]{
    var currentNode = impl_tail
    def hasNext: Boolean = currentNode ne null
    def next(): NodeType = {
      if(!hasNext) throw new DoubleLinkedList_NoSuchElementException()
      val n = currentNode
      currentNode = n.prevNode
      n
    }
  }

  def reverseIterator: Iterator[T] = new Iterator[T]{
    var currentNode = impl_tail
    def hasNext: Boolean = currentNode ne null
    def next(): T = {
      if(!hasNext) throw new DoubleLinkedList_NoSuchElementException()
      val n = currentNode
      currentNode = n.prevNode
      n.value
    }
  }

  def foreachNode[U](fun: NodeType=>U){ this.nodeIterator.foreach(fun) }
  def foreach[U](fun: T=>U){ this.iterator.foreach(fun) }

  def pushBackNode(node: NodeType): NodeType = {
    assume(node.prevNode eq null)
    assume(node.nextNode eq null)

    if(isEmpty()){
      impl_head = node
      impl_tail = node
    } else {
      node.prevNode = impl_tail

      impl_tail.nextNode = node
      impl_tail = node
    }
    node
  }

  def pushFrontNode(node: NodeType): NodeType = {
    assume(node.prevNode eq null)
    assume(node.nextNode eq null)

    if(isEmpty()){
      impl_head = node
      impl_tail = node
    } else {
      node.nextNode = impl_head

      impl_head.prevNode = node
      impl_head = node
    }
    node
  }

  def pushBack(value: T): NodeType = { this.pushBackNode( newNode(value) ) }
  def pushFront(value: T): NodeType = { this.pushFrontNode( newNode(value) ) }

  def pushBack(value1: T, value2: T, values: T*): NodeType = {
    val firstNode = this.pushBackNode( newNode(value1) )
    this.pushBackNode( newNode(value2) )
    values.foreach(v=>this.pushBackNode( newNode(v) ))

    firstNode
  }
  def pushFront(value1: T, value2: T, values: T*): NodeType = {
    val firstNode = this.pushFrontNode( newNode(value1) )
    this.pushFrontNode( newNode(value2) )
    values.foreach(v=>this.pushFrontNode( newNode(v) ))

    firstNode
  }

  def popBackNode(): NodeType = {
    if(isEmpty()) throw new DoubleLinkedList_NoSuchElementException()

    val n = impl_tail
    impl_tail = n.prevNode

    n.prevNode = null
    n
  }

  def popFrontNode(): NodeType = {
    if(isEmpty()) throw new DoubleLinkedList_NoSuchElementException()

    val n = impl_head
    impl_head = n.nextNode

    n.nextNode = null
    n
  }

  def popBack(): T = { popBackNode().value }
  def popFront(): T = { popFrontNode().value }

  def insertNodeAfter(refNode: NodeType, nodeToInsert: NodeType): NodeType = {
    assume(nodeToInsert.prevNode eq null)
    assume(nodeToInsert.nextNode eq null)

    if(refNode eq null){
      throw new DoubleLinkedList_IllegalArgumentException()
    } else {
      if(refNode.nextNode eq null){
        assume(impl_tail eq refNode)
        nodeToInsert.prevNode = refNode
        refNode.nextNode = nodeToInsert
        impl_tail = nodeToInsert
      } else {
        nodeToInsert.prevNode = refNode
        nodeToInsert.nextNode = refNode.nextNode

        refNode.nextNode.prevNode = nodeToInsert
        refNode.nextNode = nodeToInsert
      }
    }

    nodeToInsert
  }

  def insertNodeBefore(refNode: NodeType, nodeToInsert: NodeType): NodeType = {
    assume(nodeToInsert.prevNode eq null)
    assume(nodeToInsert.nextNode eq null)

    if(refNode eq null){
      throw new DoubleLinkedList_IllegalArgumentException()
    } else {
      if(refNode.prevNode eq null){
        assume(impl_head eq refNode)
        nodeToInsert.nextNode = refNode
        refNode.prevNode = nodeToInsert
        impl_head = nodeToInsert
      } else {
        nodeToInsert.nextNode = refNode
        nodeToInsert.prevNode = refNode.prevNode

        refNode.prevNode.nextNode = nodeToInsert
        refNode.prevNode = nodeToInsert
      }
    }

    nodeToInsert
  }


  def insertAfter(refNode: NodeType, value: T): NodeType = { insertNodeAfter(refNode, newNode(value)) }
  def insertBefore(refNode: NodeType, value: T): NodeType = { insertNodeBefore(refNode, newNode(value)) }

  def unlink(node: NodeType): NodeType = {

    if(node eq null) throw new DoubleLinkedList_IllegalArgumentException()

    if (node.prevNode eq null) {
      assume(impl_head eq node)
      if(node.nextNode eq null){
        assume(impl_tail eq node)
        impl_head = null
        impl_tail = null
      } else {
        impl_head = impl_head.nextNode
        impl_head.prevNode = null

        node.nextNode = null
      }
    } else if (node.nextNode eq null) {
      //assume(node.prevNode ne null)
      impl_tail = node.prevNode
      impl_tail.nextNode = null

      node.prevNode = null
    } else {
      val left  = node.prevNode
      val right = node.nextNode

      left.nextNode  = right
      right.prevNode = left

      node.prevNode = null
      node.nextNode = null
    }

    node
  }

}