package intintersections

import org.scalatest.FunSuite

import scala.util.Random

class IntersectingSubintervalsSpec extends FunSuite {

  test("query matching") {
    assert(Query.key("A") matches Set("A", "B"))
    assert(!(Query.key("C") matches Set("A", "B")))
    assert(!(Query.key("A").andNot("B") matches Set("A", "B")))
    assert(Query.key("A").andNot("C") matches Set("A", "B"))
    assert(Query.key("A").or(Query.key("B")) matches Set("A"))
  }

  test("empty query matches anything") {
    assert(Query.empty matches Set("X"))
  }

  test("query composition") {
    val q1 = Query.key("A").andNot("B")
    val q2 = Query.empty.andNot("C")
    val q3 = q2.or(q1).or(Query.key("D"))

    assert(q1.toString === "+A & -B")
    assert(q2.toString === "-C")
    assert(q3.toString === "-C | +D | +A & -B")
  }

  test("divisibility calculation on integers") {
    val N = 100
    def integer2Interval(x: Int) = Interval(x, x+1)
    def interval2Integers(i: Interval[Int]) = i.start to (i.end - 1)

    val input =
      (for(div <- 1 to N) yield
        (div, (div to N by div).map(integer2Interval))
        ).toMap

    val result = IntersectingSubintervals.of(input)

    assert(result.duration === N, "overall duration should equal")

    for(div <- 1 to N)
      assert(result.filter(Query.key(div)).duration === N / div, s"Number of integers from 1 to $N divisible by $div")

    val rnd = new Random(new java.util.Random(13))

    for(i <- 1 to 10000) { // test many random inputs
      val divisors = (0 to rnd.nextInt(Math.sqrt(N).toInt)) // 1 to sqrt(N) common divisors
        .map(_ => rnd.nextInt(N/2-1)+1) // picked from 1 to N/2
        .sorted

      val nonDivisors  = ((0 to rnd.nextInt(Math.sqrt(N).toInt)) // 1 to sqrt(N) non-divisors
        .map(_ => rnd.nextInt(N/2-1)+1) // picked from 1 to N/2
        .toSet -- divisors) // and are not already picked to be divisors
        .toIndexedSeq.sorted

      val query =
        nonDivisors.foldLeft(
          divisors.foldLeft(Query.empty[Int]) { case (q, d) => q and d }
        ) { case (q, nd) => q andNot nd }

      val numbersDivisibleAsSingleQuery = result.filter(query)

      val filteringResult = (1 to N)
        .filter(n => divisors.forall(n%_ == 0))
        .filterNot(n => nonDivisors.exists(n%_ == 0))

      assert(numbersDivisibleAsSingleQuery.duration === filteringResult.size, s" -- Number of integers from 1 to $N divisible by $divisors and not divisible by $nonDivisors")

      assert(numbersDivisibleAsSingleQuery.subintervals.flatMap(i => interval2Integers(i._2)) === filteringResult, s" -- Integers from 1 to $N divisible by $divisors and not divisible by $nonDivisors")

      val multipleSmallQueries = rnd.shuffle(divisors.map(Query.key(_)) ++ nonDivisors.map(Query.empty andNot _))

      val numbersDivisibleMultipleQueries = multipleSmallQueries.foldLeft(result) { case (r, q) => r filter q }

      assert(numbersDivisibleAsSingleQuery === numbersDivisibleMultipleQueries, s" -- Integers from 1 to $N divisible by $divisors and not divisible by $nonDivisors calculated by single query ($query) vs multiple queries ($multipleSmallQueries)")

    }
  }

  test("coalesce subintervals") {
    val input = Map(
      "A" -> Seq(Interval(0, 10)),
      "B" -> Seq(Interval(3, 5)),
      "C" -> Seq(Interval(4, 7))
    )

    val subints = IntersectingSubintervals.of(input)
    val filtered = subints.filter(Query.key("A") andNot "B")
    val coalesced = filtered.coalesce(Set("A"))

    assert(filtered
      === IntersectingSubintervals(Seq((Set("A"),Interval(0,3)), (Set("A", "C"),Interval(5,7)), (Set("A"),Interval(7,10)))))

    assert(coalesced
      === IntersectingSubintervals(Seq((Set("A"),Interval(0,3)), (Set("A"),Interval(5,10)))))

    assert(filtered !== coalesced)

    assert(filtered.duration === coalesced.duration)
  }

}
