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


class BinaryTreeSet extends Actor{
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
    case op: Operation => root ! op 
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }          
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => {
      pendingQueue = pendingQueue.enqueue(op)
    }
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      pendingQueue foreach (root ! _)
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
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
    
    
    //if this node is not a leaf, delegate action to subtree
    case op: Operation if (op.elem < elem && subtrees.isDefinedAt(Left)) => subtrees(Left) ! op
    case op: Operation if (op.elem > elem && subtrees.isDefinedAt(Right)) => subtrees(Right) ! op
    
    
    //following action only occur in leaf
    case Insert(req, id, el) => {
      if (el == elem) {
        removed = false
      }
      else if (el < elem) {
        val node = context.actorOf(props(el, false))
        subtrees = subtrees + ((Left, node))        
      }
      else if (el > elem) {
        val node = context.actorOf(props(el, false))
        subtrees = subtrees + ((Right, node))        
      }
      req ! OperationFinished(id)      
    }
    
    case Contains(req, id, el) => {
      if (el == elem){ 
        req ! ContainsResult(id, !removed)
      }
      else{
        req ! ContainsResult(id, false)
      }
    }
    
    case Remove(req, id, el) => {
      if(el == elem){
        removed = true
      }
      req ! OperationFinished(id)
    }
    
    case CopyTo(newRoot) => {
      if (!removed) newRoot ! Insert(self, elem, elem)
      subtrees.values.foreach( (subtree) => subtree ! CopyTo(newRoot))
      context.become(copying(subtrees.values.toSet, insertConfirmed=removed))
    }
        
    
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    if (expected.isEmpty && insertConfirmed) {            
      context.parent ! CopyFinished                 
      normal
    }
    else {
      case OperationFinished(id) => if (id == elem) context.become(copying(expected, insertConfirmed = true))
      case CopyFinished => context.become(copying(expected - sender, insertConfirmed))
    }
  }


}
