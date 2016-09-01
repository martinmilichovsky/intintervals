package intintersections

import org.scalatest.FunSuite

import scala.util.Random

class SubintervalsCalculatorSpec extends FunSuite {

  import Query._
  test("union overlapping") {
    val input = Seq(
      Interval(0, 3),
      Interval(1, 2),
      Interval(2, 4),
      Interval(6, 10),
      Interval(14, 16),
      Interval(13, 18)
    )

    val unioned = SubintervalsCalculator.unionOverlapping(scala.util.Random.shuffle(input)).toSet
    assert(unioned ===
      Set(Interval(0, 4), Interval(6, 10), Interval(13, 18))
    )
  }

  test("union overlapping: big random input") {
    val N = 500
    val randomInput = for (i <- 0 to N) yield {
      val a = Math.abs(Random.nextInt()) % (N * 10)
      val b = a + Math.abs(Random.nextInt()) % (N / 100 + 1)
      Interval(a, b)
    }
    val startTime = System.currentTimeMillis()
    val unioned = SubintervalsCalculator.unionOverlapping(randomInput)
    val elapsedTime = System.currentTimeMillis() - startTime

    //println(s"From ${randomInput.size} intervals flattened to ${unioned.size} in $elapsedTime milliseconds")

    // brute-force verification that no two intervals actually overlap
    val intersecting = unioned.toList.combinations(2).filter {
      case a :: b :: Nil => a canUnion b
      case _ => ???
    }
    assert(intersecting.isEmpty)
  }

  test("simple example") {
    val input = Map(
      "A" -> Seq(Interval(0, 1), Interval(3, 4), Interval(6, 7)),
      "B" -> Seq(Interval(0, 3), Interval(5, 8))
    )
    val output = SubintervalsCalculator.calculateSubintervals(input)
    assert(output.subintervals ===
      Seq(
        (Set("A", "B"),Interval(0,1)),
        (Set("B"),Interval(1,3)),
        (Set("A"),Interval(3,4)),
        (Set(),Interval(4,5)),
        (Set("B"),Interval(5,6)),
        (Set("B", "A"),Interval(6,7)),
        (Set("B"),Interval(7,8))
      )
    )

    /**
      *INPUT:
      *012345678
      *Aa.Aa.Aa.
      *BBBb.BBBb
      *
      *filter +A:
      *Aa.Aa.Aa
      *
      *filter +A & -B:
      *...Aa....
      *012345678
      *
      */

    assert(output.filter("A").subintervals.map(_._2)
      === input("A"))

    assert(output.filter("A" andNot "B").subintervals
      === Seq((Set("A"), Interval(3, 4))))

    assert(output.filter("A" or "B").subintervals
      === output.subintervals.filter(_._1.nonEmpty))

  }

}
