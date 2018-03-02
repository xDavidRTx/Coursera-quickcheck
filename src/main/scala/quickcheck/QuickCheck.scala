package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] =

    for {
      k <- arbitrary[A]
      p <- oneOf(genHeap, const(empty))
    } yield insert(k, p)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("prop 1") = forAll { (h: H) =>
    val r = scala.util.Random
    val fst = r.nextInt(100)
    val scnd = r.nextInt(100)
    val m = insert(fst,insert(scnd,empty))
    if(fst > scnd) findMin(m) == scnd
    else findMin(m) == fst
  }

  property("prop 2") = forAll { (h: H) =>

      val r = scala.util.Random
      deleteMin(insert(r.nextInt(50), empty)) == empty

  }


  /*Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  (Hint: recursion and helper functions are your friends.)
   */
  property("prop 3") = forAll { (h: H) =>
    def getElm (init: H, acc: List[Int]):  List[Int] = {
      if(init == empty) acc
      else {
        getElm(deleteMin(init), findMin(init) :: acc)
      }
    }
    val list = getElm(h , Nil)
    list == list.sorted
  }


  //  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("prop 4") = forAll { (h1: H , h2:H) =>

    val min_h1 = findMin(h1)
    val min_h2 = findMin(h2)
    val min_melt = findMin(meld(h1,h2))
    if(min_h1< min_h2 )min_melt == min_h1
    else min_melt == min_h2
  }

}
