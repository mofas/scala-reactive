package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.language.postfixOps
import scala.concurrent.duration._
import akka.actor.Cancellable

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var snapshotRepeaters = Map.empty[Long, Cancellable]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case replicate @ Replicate(key, valueOpt, id) => {
      val seq = nextSeq
      acks += seq -> (sender, replicate)
      snapshotRepeaters += (seq -> context.system.scheduler.schedule(0 milli, 250 millis, replica, Snapshot(key, valueOpt, seq)))      
    }
            
    case SnapshotAck(key, seq) => {
      if (acks.contains(seq)) {      
        val (sender, Replicate(key, _, id)) = acks(seq)
        acks -= seq
        snapshotRepeaters(seq).cancel
        snapshotRepeaters -= seq
        sender ! Replicated(key, id)
      }            
    }
  }

}
