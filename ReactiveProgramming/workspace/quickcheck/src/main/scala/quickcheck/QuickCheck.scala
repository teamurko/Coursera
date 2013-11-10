package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.math.{min, max, abs}
import scala.Int
import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min_of_size_one") = forAll { a: Int =>
  	val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min_of_size_two") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == min(a, b)
  }

  property("min_of_melded_heaps") = forAll { (h1 : H, h2 : H) =>
  	(!isEmpty(h1) && !isEmpty(h2)) ==> (min(findMin(h1), findMin(h2)) == findMin(meld(h1, h2)))
  }
    
  property("ordered_list") = forAll { h : H =>
    val list = toList(h)
    list.sameElements(list.sorted) && list.sorted.sameElements(toList(fromList(list.reverse)))
  }
  
  property("delete_min_empty") = forAll { a : A =>
    val h = insert(a, empty)
    findMin(h) == a && isEmpty(deleteMin(h))
  }
  
  def fromList(l : List[A]) : H = l match {
    case Nil => empty
    case t::ts => insert(t, fromList(ts))
  }
  
  def toList(h : H) : List[A] = {
      if (isEmpty(h)) List.empty
      else findMin(h) :: toList(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
	h <- frequency((1, value(empty)), (50, genHeap))
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
