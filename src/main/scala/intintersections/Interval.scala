package intintersections

case class Interval[BOUND](start: BOUND, end: BOUND)(implicit ord: Ordering[BOUND])
{
  assert(ord.compare(start, end) <= 0, s"start shouldn't be after end ($start > $end)")

  def overlaps(other: Interval[BOUND]): Boolean =
    this.end != other.start && canUnion(other)

  def canUnion(other: Interval[BOUND]): Boolean =
    ord.compare(start, other.end) <= 0 && ord.compare(end, other.start) >= 0

  def intersect(other: Interval[BOUND]) = if (overlaps(other))
    Interval(
      if(ord.compare(start, other.start) >= 0) start else other.start,
      if(ord.compare(other.end, end) >= 0) end else other.end
    )
  else
    throw new IllegalArgumentException("cannot intersect non-overlapping intervals")

  def union(other: Interval[BOUND]) = if (canUnion(other))
    Interval(
      if(ord.compare(start, other.start) <= 0) start else other.start,
      if(ord.compare(other.end, end) <= 0) end else other.end
    )
  else
    throw new IllegalArgumentException("cannot union non-overlapping/following intervals")

}
