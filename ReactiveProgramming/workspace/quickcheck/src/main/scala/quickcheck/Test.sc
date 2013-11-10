package quickcheck

import common._;
import org.scalacheck._;
import quickcheck._;

object Test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  //object MyAppSpecification extends Properties("MyApp") {
	//	include(QuickCheckHeap)
	//}
	(0 until 10).toList                       //> res0: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	val g = Gen.containerOf[List, Int](Gen.oneOf(0, 10));
                                                  //> g  : org.scalacheck.Gen[List[Int]] = Gen()
}