package intintersections

import scala.annotation.tailrec

object SubintervalsCalculator {

  def calculateSubintervals[KEY, BOUND, DURATION](intervalGroups: Map[KEY, Seq[Interval[BOUND]]])
                                                 (implicit ord: Ordering[BOUND]): IntersectingSubintervals[KEY, BOUND] = {

    val flattenedByKey = intervalGroups.toSeq.flatMap {
      case (key, events) => unionOverlapping(events).map((key, _))
    }

    if (flattenedByKey.isEmpty)
      return IntersectingSubintervals(Seq.empty) // nothing to do

    val starts = flattenedByKey.map {
      case (key, Interval(start, _)) => (start, key)
    }.sortBy(_._1)
    val ends = flattenedByKey.map {
      case (key, Interval(_, end)) => (end, key)
    }.sortBy(_._1)

    @tailrec
    def calculateSubintervalsRec(activeKeys: Set[KEY],
                                 lastMark: BOUND,
                                 nearestStarts: List[(BOUND, KEY)],
                                 nearestEnds: List[(BOUND, KEY)],
                                 acc: List[(Set[KEY], Interval[BOUND])] = List.empty): List[(Set[KEY], Interval[BOUND])] = {
      (nearestEnds, nearestStarts) match {
        case (Nil, Nil) => acc
        case (ends, start :: startXs) if ends.isEmpty || ord.compare(ends.head._1, start._1) > 0 =>
          calculateSubintervalsRec(
            activeKeys = activeKeys + start._2,
            lastMark = start._1,
            nearestStarts = startXs,
            nearestEnds = ends,
            acc = (activeKeys, Interval(lastMark, start._1)) :: acc)
        case (end :: endXs, starts) =>
          calculateSubintervalsRec(
            activeKeys = activeKeys - end._2,
            lastMark = end._1,
            nearestStarts = starts,
            nearestEnds = endXs,
            acc = (activeKeys, Interval(lastMark, end._1)) :: acc)
      }
    }

    IntersectingSubintervals(calculateSubintervalsRec(activeKeys = Set.empty,
      lastMark = ord.min(starts.head._1, ends.head._1), // for every start, there is an end, so this is safe
      nearestStarts = starts.toList,
      nearestEnds = ends.toList,
      Nil
    ).filterNot {
      case (key, Interval(start, end)) => ord.compare(start, end) >= 0
    }.reverse)
  }

  def unionOverlapping[BOUND](events: Seq[Interval[BOUND]])(implicit ord: Ordering[BOUND]): Seq[Interval[BOUND]] =
    events.sortBy(_.start).foldLeft(List.empty[Interval[BOUND]]) {
      case (last :: xs, current) if last canUnion current => (last union current) :: xs
      case (xs, current) => current :: xs
    }

}