package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
 

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  

  property("insert 2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    if(a < b){
      findMin(h) == a
    }
    else{
      findMin(h) == b
    }
  }
  
  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }
  
  def heapToList(heap: H): List[Int] =
    if (isEmpty(heap)) Nil else findMin(heap) :: heapToList(deleteMin(heap))
  
  def isSorted(h: H): Boolean = {
     val l1 = heapToList(h)
     l1.sorted == l1     
  }  
  
  
  property("sort") = forAll { (h: H) =>
    isSorted(h)    
  }
  
  property("meld min") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    val min = findMin(h3)
    min == findMin(h1) || min == findMin(h2)
  }
  
  property("meld sort") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    isSorted(h3)   
  }
  
  property("meld sort 2") = forAll { (h1: H, h2: H) =>
    val l1 = heapToList(h1)
    val l2 = heapToList(h2)
    val h3 = meld(h1, h2)
    val l3 = heapToList(h3)
    (l1 ++ l2).sorted == l3 
  }
 
  
  

  
  

}
