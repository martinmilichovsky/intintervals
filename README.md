# intintervals (Intersections of Intervals) [![Build Status](https://travis-ci.org/martinmilichovsky/intintervals.svg?branch=master)](https://travis-ci.org/martinmilichovsky/intintervals)

intintervals is library for calculating set intersections cardinality on a timeline. User provides several timelines and then can query for overlaps.

Interval
---
Intervals in this library are considered Half-Closed, where begining belongs to the interval,  while endpoint doesn't belong to the interval. So `Interval(0, 1) overlaps Interval(1, 2) == false`

The overlapping intersections are calculated by sweep line algorithm in linear time. The querying is also linear. 

It is possible to use `IntersectingSubintervals` for the results. This has information about the actual bounds. While `SubintervalsDurationTable` has only aggregated durations.

For `Numeric` bounds of intervals there are implicits, that in other cases you'd need to provide: `Ordering` for bounds, and if you want to use the durations you would have to provide also `ExtractDuration` and `SumDuration`.

Better see the code for more insight.

Example
---
```scala
scala> import intintersections.{Interval, IntersectingSubintervals, Query}
import intintersections.{Interval, IntersectingSubintervals, Query}

scala> val input = Map(
  "A" -> Seq(Interval(0, 1), Interval(2, 3)),
  "B" -> Seq(Interval(0, 2), Interval(4, 6)),
  "C" -> Seq(Interval(0, 3), Interval(6, 9))
)

scala> val intint = IntersectingSubintervals.of(input)
intint: intintersections.IntersectingSubintervals[String,Int] = IntersectingSubintervals(List((Set(A, B, C),Interval(0,1)), (Set(B, C),Interval(1,2)), (Set(C, A),Interval(2,3)), (Set(),Interval(3,4)), (Set(B),Interval(4,6)), (Set(C),Interval(6,9))))

scala> import intintersections.Query._ // implicits for Query construction
import intintersections.Query._

scala> intint.filter("B" and "C")
res1: intintersections.IntersectingSubintervals[String,Int] = IntersectingSubintervals(List((Set(A, B, C),Interval(0,1)), (Set(B, C),Interval(1,2))))

scala> intint.filter("B" and "C" andNot "A")
res2: intintersections.IntersectingSubintervals[String,Int] = IntersectingSubintervals(List((Set(B, C),Interval(1,2))))

scala> intint.filter("B" and "C" andNot "A").duration
res3: Int = 1
```

Use Case
---

My usecase was to calculate time spent working of employees according to labour law: different tarrif at nights, weekends, weekend nights, holidays. Also quantify breaks which had to occus after some time of continuuous work. 
So I collected employee's attendance records, broke them to work/pause _types_ and then for the period considered I generated _calendar_ timeline. Then I could query for `work & SATURDAY & !night` etc. For more detail see `TimeImplicitsSpec` test.

## License

Copyright © 2016 Martin Milichovsky

Distributed under the Apache License, Version 2.0
