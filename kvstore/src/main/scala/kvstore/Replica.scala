package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.actor.Cancellable
import scala.language.postfixOps
import scala.concurrent.duration._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
    
  case class PersistTimeout(id: Long)
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  
  //create a persistence storage for the replica
  val persistence = context.actorOf(persistenceProps)
      
  var persistAcks = Map.empty[Long, ActorRef]
  var replicateAcks = Map.empty[Long, (ActorRef, Set[ActorRef])]
  var persistRepeaters = Map.empty[Long, Cancellable]
  var persistFailures = Map.empty[Long, Cancellable]  
  var snapshotSeq = 0L
  
  
  def makePersist(key: String, valueOpt: Option[String], id: Long) = {        
    persistAcks += (id -> sender)
    if (replicators.nonEmpty) {
      replicateAcks += (id -> (sender, replicators))
      replicators.foreach(_ ! Replicate(key, valueOpt, id))
    }    
    persistRepeaters += (id -> context.system.scheduler.schedule(0 millis, 100 millis, persistence, Persist(key, valueOpt, id)))
    persistFailures += (id -> context.system.scheduler.scheduleOnce(1000 millis) {
      self ! PersistTimeout(id)
    })
  }
  
  
  arbiter ! Join


  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    
    case Get(key, id) => {      
      sender ! GetResult(key, kv.get(key), id)
    }
    
    case Insert(key, value, id) => {
      kv += (key -> value)                 
      makePersist(key, Some(value), id)
    }
    
    case Remove(key, id) => {
      kv -= key
      makePersist(key, None, id)      
    }
    
    case Persisted(key, id) =>{       
      val originalSender = persistAcks(id)      
      persistAcks -= id
      persistRepeaters(id).cancel
      persistRepeaters -= id
      
      if (!replicateAcks.contains(id)) {
        persistFailures(id).cancel
        persistFailures -= id
        originalSender ! OperationAck(id)
      }      
      
    }
    
    case PersistTimeout(id) => {    
      if (persistFailures.contains(id)) {
        if (persistRepeaters.contains(id)) {
          persistRepeaters(id).cancel
          persistRepeaters -= id
        }
        var originalSender =
        if (persistAcks.contains(id)){
          persistAcks(id)
        }        
        else {
          replicateAcks(id)._1
        }                       
        originalSender ! OperationFailed(id)
        persistFailures -= id
        persistAcks -= id
            
     }
    }
   
    
    //Protocol event
    case Replicated(key, id) => {
      if (replicateAcks.contains(id)) {
        val (originalSender, replicators) = replicateAcks(id)
        val newAckSet = replicators - sender
        if (newAckSet.isEmpty) {
          replicateAcks -= id
          if (!persistAcks.contains(id)) {
            persistFailures(id).cancel
            persistFailures -= id          
            originalSender ! OperationAck(id)
          } 
        }
        else {
          replicateAcks = replicateAcks.updated(id, (originalSender, newAckSet))
        }        
      }
    }
    
    case Replicas(replicas) => {
        val newReplicas = replicas.filterNot(_ == self)        
        val enterReplicas = newReplicas -- secondaries.keySet
        val exitReplicas = secondaries.keySet -- newReplicas
        
        var enterSecondaries = Map.empty[ActorRef, ActorRef]
        
        //create new replicator
        val enterReplicators = enterReplicas.map { 
          replica => {
            val replicator = context.actorOf(Replicator.props(replica))
            enterSecondaries += (replica -> replicator)
            replicator
          }
        }        

        //tell all dead replicator to suicide   
        exitReplicas.foreach { 
          replica => {
            secondaries(replica) ! PoisonPill
            replicateAcks.foreach {
              case (id, (_, replicator)) =>
                if (replicator.contains(secondaries(replica))) {
                  self.tell(Replicated("", id), secondaries(replica))
                  //cannot self ! ....fuck..
                  //self ! (Replicated("", id), secondaries(replica))
                }
            }
          }
        }        
        
        val exitReplicators = exitReplicas map secondaries
        
        replicators = replicators ++ enterReplicators -- exitReplicators
        secondaries = secondaries ++ enterSecondaries -- exitReplicas
        
        enterReplicators.foreach { replicator =>
          kv.zipWithIndex.foreach {
            case ((k, v), id) => {
              replicator ! Replicate(k, Some(v), id)
            }
          }
        }
    }
    
    
    
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    
    case Get(key, id) => {
      sender ! GetResult(key, kv get key, id)
    }
    
    //Protocol event    
    case Snapshot(key, valueOption, seq) => {      
      if (seq < snapshotSeq) {
        sender ! SnapshotAck(key, seq)
      }
      else if (seq == snapshotSeq) {
        valueOption match {
          case None => kv -= key
          case Some(value) => kv += key -> value
        }
        snapshotSeq += 1
        persistAcks += seq -> sender
        persistRepeaters += seq -> context.system.scheduler.schedule(0 milli, 100 millis, persistence, Persist(key, valueOption, seq))        
      }      
    }
    
    case Persisted(key, id) => {
      val sender = persistAcks(id)
      persistAcks -= id
      persistRepeaters(id).cancel
      persistRepeaters -= id
      sender ! SnapshotAck(key, id)
    }
  }

}

