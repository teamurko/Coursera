/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]
  
  // optional
  def receive = normal
  
  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case o: Operation => {
      root ! o
    }
    case GC => {
      var newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case o: Operation => {
      pendingQueue = pendingQueue :+ o
    }
    case CopyFinished => {
      context.stop(root)
      root = newRoot
      while (!pendingQueue.isEmpty) {
        root ! pendingQueue.head
        pendingQueue = pendingQueue.tail
      }
      context.become(normal)
    }
    case GC => {
      // ignore
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester: ActorRef, id: Int, newElement: Int) => {
      if (newElement == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else if (newElement < elem) {
        val child = subtrees.get(Left)
        if (child.isEmpty) {
          val newNode = context.actorOf(props(newElement, false))
          subtrees = subtrees.updated(Left, newNode)
          requester ! OperationFinished(id)
        } else {
          child.get ! Insert(requester, id, newElement)
        }
      } else {
        val child = subtrees.get(Right)
        if (child.isEmpty) {
          subtrees = subtrees.updated(Right, context.actorOf(props(newElement, false)))
          requester ! OperationFinished(id)
        } else {
          child.get ! Insert(requester, id, newElement)
        }
      }
    }
    case Remove(requester: ActorRef, id: Int, newElement: Int) => {
      if (newElement == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val child = {
          if (newElement < elem) subtrees.get(Left)
          else subtrees.get(Right)
        }
        if (child.isEmpty) {
          requester ! OperationFinished(id)
        } else {
          child.get ! Remove(requester, id, newElement)
        }
      }
    }
    case Contains(requester: ActorRef, id: Int, newElement: Int) => {
      if (newElement == elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        val child = {
          if (newElement < elem) subtrees.get(Left)
          else subtrees.get(Right)
        }
        if (child.isEmpty) {
          requester ! ContainsResult(id, false)
        } else {
          child.get ! Contains(requester, id, newElement)
        }
      }
    }
    case CopyTo(treeNode: ActorRef) => {
      var expected = Set.empty[ActorRef]
      if (!subtrees.get(Left).isEmpty) {
        expected = expected + subtrees.get(Left).get
      }
      if (!subtrees.get(Right).isEmpty) {
        expected = expected + subtrees.get(Right).get
      }
      if (removed) {
        if (expected.isEmpty) {
          context.parent ! CopyFinished
        } else {
    	  context.become(copying(expected, true))
        }
      } else {
        context.become(copying(expected, false))
        treeNode ! Insert(self, -1, elem)
      }
      var child = subtrees.get(Left)
      if (!child.isEmpty) {
        child.get ! CopyTo(treeNode)
      }
      child = subtrees.get(Right)
      if (!child.isEmpty) {
        child.get ! CopyTo(treeNode)
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) => {
      if (expected.isEmpty) {
       context.parent ! CopyFinished
      } else {
        context.become(copying(expected, true))
      }
    }
    case CopyFinished => {
      val nextExpected = expected - context.sender
      if (nextExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(nextExpected, insertConfirmed))
      }
    }
  }

}
