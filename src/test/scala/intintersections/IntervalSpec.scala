package intintersections

import org.scalatest.FunSuite

class IntervalSpec extends FunSuite {

  test("interval validity") {
    intercept[AssertionError](Interval(2, 0))
  }

  test("interval connectivity/intersecting") {
    assert((Interval(0, 1) overlaps Interval(1, 2)) === false)

    assert((Interval(1, 2) canUnion Interval(3, 4)) === false)
    assert((Interval(1, 2) canUnion Interval(3, 4)) === false)
    assert((Interval(0, 1) canUnion Interval(1, 2)) === true)

    assert(Interval(1, 2) canUnion Interval(1, 2))

    assert(Interval(1, 5) canUnion Interval(3, 6))
    assert(Interval(1, 5) canUnion Interval(2, 4))
    assert(Interval(1, 5) canUnion Interval(0, 3))
    assert(Interval(1, 5) canUnion Interval(0, 6))

  }

  test("interval intersection") {
    assert((Interval(1, 5) intersect Interval(3, 6)) === Interval(3, 5))
    assert((Interval(1, 5) intersect Interval(2, 4)) === Interval(2, 4))
    assert((Interval(1, 5) intersect Interval(0, 3)) === Interval(1, 3))
    assert((Interval(1, 5) intersect Interval(0, 6)) === Interval(1, 5))

    intercept[IllegalArgumentException](Interval(0, 1) intersect Interval(2, 3))
  }

  test("interval union") {
    assert((Interval(1, 5) union Interval(3, 6)) === Interval(1, 6))
    assert((Interval(1, 5) union Interval(2, 4)) === Interval(1, 5))
    assert((Interval(1, 5) union Interval(0, 3)) === Interval(0, 5))
    assert((Interval(1, 5) union Interval(0, 6)) === Interval(0, 6))

    intercept[IllegalArgumentException](Interval(0, 1) union Interval(2, 3))
  }

}
