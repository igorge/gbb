package gie.ut

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import gie.collection.double_linked_list._

class DoubleLinkedListTest extends FlatSpec with PrivateMethodTester {

  "Double linked list" should "construct" in {
    val dll = new DoubleLinkedList[Int]()

    dll.isEmpty() should equal (true)
    evaluating{ dll.head } should produce [NoSuchElementException]
    evaluating{ dll.tail } should produce [NoSuchElementException]
  }

  it should "construct throw exceptions for empty list" in {
    val dll = new DoubleLinkedList[Int]()

    val iter = dll.nodeIterator

    iter.hasNext should equal (false)
    evaluating{ iter.next() } should produce [NoSuchElementException]
    evaluating{ iter.next() } should produce [DoubleLinkedListException]
    evaluating{ iter.next() } should produce [NoSuchElementException with DoubleLinkedListException]
  }

  it should "construct default node" in {
    val dll = new DoubleLinkedList[Int]()

    val node = dll.newNode()
    classOf[DefaultNode[Int]].isInstance(node) should equal (true)
    node.nextNode should equal (null)
    node.prevNode should equal (null)

    val node2 = dll.newNode(42)
    node2.value should equal (42)
    classOf[DefaultNode[Int]].isInstance(node2) should equal (true)
    node2.nextNode should equal (null)
    node2.prevNode should equal (null)

    dll.isEmpty() should equal (true)
  }

  it should "pushBack() nodes" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushBack(1)
    dll.isEmpty() should equal (false)

    val iter1 = dll.nodeIterator
    iter1.hasNext should equal (true)
    iter1.next().value should equal (1)
    iter1.hasNext should equal (false)

    val iter2 = dll.iterator
    iter2.hasNext should equal (true)
    iter2.next() should equal (1)
    iter2.hasNext should equal (false)


    dll.pushBack(2)
    dll.pushBack(3)
    dll.isEmpty() should equal (false)
    val iter3 = dll.iterator
    iter3.next() should equal(1)
    iter3.next() should equal(2)
    iter3.next() should equal(3)
    evaluating{ iter3.next() } should produce [NoSuchElementException]

    val iter4 = dll.iterator
    iter4.next() should equal(1)
    iter4.next() should equal(2)
    iter4.next() should equal(3)
    evaluating{ iter4.next() } should produce [NoSuchElementException]

  }


  it should "pushFront() nodes" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushFront(1)
    dll.isEmpty() should equal (false)

    val iter1 = dll.nodeIterator
    iter1.hasNext should equal (true)
    iter1.next().value should equal (1)
    iter1.hasNext should equal (false)

    val iter2 = dll.iterator
    iter2.hasNext should equal (true)
    iter2.next() should equal (1)
    iter2.hasNext should equal (false)


    dll.pushFront(2)
    dll.pushFront(3)
    dll.isEmpty() should equal (false)
    val iter3 = dll.iterator
    iter3.next() should equal(3)
    iter3.next() should equal(2)
    iter3.next() should equal(1)
    evaluating{ iter3.next() } should produce [NoSuchElementException]

    val iter4 = dll.iterator
    iter4.next() should equal(3)
    iter4.next() should equal(2)
    iter4.next() should equal(1)
    evaluating{ iter4.next() } should produce [NoSuchElementException]
  }

  it should "have working iterators" in {
    val data = (1 to 10).toArray

    val dll = new DoubleLinkedList[Int]()
    data.foreach( dll.pushBack(_) )

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }
    evaluating{ iter1.next() } should produce [NoSuchElementException]

    val iter1_1 = dll.nodeIterator
    data.foreach{ v=> iter1_1.next().value should equal (v) }
    evaluating{ iter1_1.next() } should produce [NoSuchElementException]

    val iter2 = dll.reverseIterator
    data.reverse.foreach{ v=> iter2.next() should equal (v) }
    evaluating{ iter2.next() } should produce [NoSuchElementException]

    val iter2_1 = dll.reverseNodeIterator
    data.reverse.foreach{ v=> iter2_1.next().value should equal (v) }
    evaluating{ iter2_1.next() } should produce [NoSuchElementException]

  }

  it should "insertAfter() for empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = (0 to 1).toArray

    evaluating{ dll.insertAfter(dll.head, 1) } should produce [NoSuchElementException]
    evaluating{ dll.insertAfter(null, 1) } should produce [IllegalArgumentException]

    dll.pushBack(0)
    dll.insertAfter(dll.head, 1).value should equal(1)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }
  }

  it should "insertAfter() for non empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = (0 to 3).toArray

    dll.pushBack(0)

    val insertedNode = dll.insertAfter(dll.tail, 1)
    insertedNode.value should equal (1)
    evaluating{ dll.insertAfter(null, 1) } should produce [IllegalArgumentException]

    dll.insertAfter(dll.tail, 2).value should equal (2)
    dll.insertAfter(dll.tail, 3).value should equal (3)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }

    val iter2 = dll.reverseIterator
    data.reverse.foreach{ v=> iter2.next() should equal (v) }
  }


  it should "insertAfter() in middle for non empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = Array(0,4,3,2,1)

    dll.pushBack(0)

    dll.insertAfter(dll.head, 1).value should equal (1)
    dll.insertAfter(dll.head, 2).value should equal (2)
    dll.insertAfter(dll.head, 3).value should equal (3)
    dll.insertAfter(dll.head, 4).value should equal (4)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }

    val iter2 = dll.reverseIterator
    data.reverse.foreach{ v=> iter2.next() should equal (v) }
  }


  it should "insertBefore() for empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = (0 to 1).reverse.toArray

    evaluating{ dll.insertBefore(dll.head, 1) } should produce [NoSuchElementException]
    evaluating{ dll.insertBefore(null, 1) } should produce [IllegalArgumentException]

    dll.pushBack(0)
    dll.insertBefore(dll.tail, 1).value should equal(1)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }
  }

  it should "insertBefore() for non empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = (0 to 3).reverse.toArray

    dll.pushBack(0)

    val insertedNode = dll.insertBefore(dll.head, 1)
    insertedNode.value should equal (1)
    evaluating{ dll.insertBefore(null, 1) } should produce [IllegalArgumentException]

    dll.insertBefore(dll.head, 2).value should equal (2)
    dll.insertBefore(dll.head, 3).value should equal (3)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }

    val iter2 = dll.reverseIterator
    data.reverse.foreach{ v=> iter2.next() should equal (v) }
  }


  it should "insertBefore() in middle for non empty list" in {
    val dll = new DoubleLinkedList[Int]()
    val data = Array(1,2,3,4,0)

    dll.pushBack(0)

    dll.insertBefore(dll.tail, 1).value should equal (1)
    dll.insertBefore(dll.tail, 2).value should equal (2)
    dll.insertBefore(dll.tail, 3).value should equal (3)
    dll.insertBefore(dll.tail, 4).value should equal (4)

    val iter1 = dll.iterator
    data.foreach{ v=> iter1.next() should equal (v) }

    val iter2 = dll.reverseIterator
    data.reverse.foreach{ v=> iter2.next() should equal (v) }
  }

  it should "throw on popBack() on empty list" in {
    val dll = new DoubleLinkedList[Int]()
    evaluating{ dll.popBack() } should produce[NoSuchElementException]
  }

  it should "popBack()" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushBack(1,2,3,4).value should equal(1)

    {val iter1 = dll.iterator;   (1 to 4).foreach{ v=> iter1.next() should equal (v) } }
    dll.popBack() should equal (4)

    dll.pushBack(5)

    {val iter1 = dll.iterator; ((1 to 3) ++ (5 to 5)).foreach{ v=> iter1.next() should equal (v) }}
    {val iter1 = dll.reverseIterator; ((1 to 3) ++ (5 to 5)).reverse.foreach{ v=> iter1.next() should equal (v) }}

    dll.popBack()
    dll.popBack()

    {val iter1 = dll.iterator; (1 to 2).foreach{ v=> iter1.next() should equal (v) }}
    {val iter1 = dll.reverseIterator; (1 to 2).reverse.foreach{ v=> iter1.next() should equal (v) }}

  }


  it should "throw on popFront() on empty list" in {
    val dll = new DoubleLinkedList[Int]()
    evaluating{ dll.popFront() } should produce[NoSuchElementException]
  }


  it should "popFront()" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushBack(1,2,3,4).value should equal(1)

    {val iter1 = dll.iterator;   (1 to 4).foreach{ v=> iter1.next() should equal (v) } }
    dll.popFront() should equal (1)

    dll.pushFront(5)

    {val iter1 = dll.iterator; ((5 to 5) ++ (2 to 4)).foreach{ v=> iter1.next() should equal (v) }}
    {val iter1 = dll.reverseIterator; ((5 to 5) ++ (2 to 4)).reverse.foreach{ v=> iter1.next() should equal (v) }}

    dll.popFront()
    dll.popFront()

    {val iter1 = dll.iterator; (3 to 4).foreach{ v=> iter1.next() should equal (v) }}
    {val iter1 = dll.reverseIterator; (3 to 4).reverse.foreach{ v=> iter1.next() should equal (v) }}

  }


  it should "unlink() for first and only" in {
    val dll = new DoubleLinkedList[Int]()

    evaluating{ dll.unlink(null) } should produce[IllegalArgumentException]
    evaluating{ dll.unlink(dll.newNode()) } should produce[AssertionError]

    val node = dll.pushBack(42)
    dll.unlink( node ).value should equal (42)
    dll.isEmpty() should equal (true)
  }

  it should "unlink() for first" in {
    val dll = new DoubleLinkedList[Int]()

    val node = dll.pushBack(1)
    dll.pushBack(2, 3)

    dll.unlink( node ).value should equal (1)
    (dll.iterator.toArray.toSeq == Array(2, 3).toSeq ) should equal (true)
    (dll.reverseIterator.toArray.toSeq == Array(3, 2).toSeq ) should equal (true)
  }

  it should "unlink() for last" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushBack(1, 2)
    val node = dll.pushBack(3)

    dll.unlink( node ).value should equal (3)
    (dll.iterator.toArray.toSeq == Array(1, 2).toSeq ) should equal (true)
    (dll.reverseIterator.toArray.toSeq == Array(2, 1).toSeq ) should equal (true)
  }


  it should "unlink() for middle" in {
    val dll = new DoubleLinkedList[Int]()

    dll.pushBack(1, 2)
    val node = dll.pushBack(3)
    dll.pushBack(4)

    dll.unlink( node ).value should equal (3)
    (dll.iterator.toArray.toSeq == Array(1, 2, 4).toSeq ) should equal (true)
    (dll.reverseIterator.toArray.toSeq == Array(4, 2, 1).toSeq ) should equal (true)
  }

}