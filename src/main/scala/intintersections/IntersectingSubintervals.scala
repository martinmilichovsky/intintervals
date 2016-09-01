package intintersections


case class IntersectingSubintervals[KEY, BOUND](subintervals: Seq[(Set[KEY], Interval[BOUND])])(implicit ord: Ordering[BOUND]) {

  def filter(key: KEY): IntersectingSubintervals[KEY, BOUND] = filter(Query.key(key))
  def filter(query: Query[KEY]): IntersectingSubintervals[KEY, BOUND] =
    copy(subintervals = subintervals.filter(query matches _._1))

  def coalesce(preserveKeys: Set[KEY]) = copy(subintervals =
    subintervals
      .map {
        case (keys, interval) => (keys intersect preserveKeys, interval)
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toSeq
      .flatMap {
        case (keys, ints) => SubintervalsCalculator.unionOverlapping(ints).map((keys, _))
      }
      .sortBy(_._2.start)
  )

  def toTable[DURATION](implicit extract: ExtractDuration[BOUND, DURATION], sum: SumDuration[DURATION]): SubintervalsDurationTable[KEY, DURATION] =
    SubintervalsDurationTable(
      subintervals
        .map {
          case (keys, interval) => (keys, extract.length(interval.start, interval.end))
        }
        .groupBy(_._1)
        .mapValues(_.map(_._2).reduce(sum.sum))
    )

  def duration[DURATION](implicit extract: ExtractDuration[BOUND, DURATION], sum: SumDuration[DURATION]) = toTable.duration

}

case class SubintervalsDurationTable[KEY, DURATION](subintervals: Map[Set[KEY], DURATION])(implicit sum: SumDuration[DURATION]) {

  def filter(query: Query[KEY]) = copy(subintervals = subintervals.filter {
    case (keys, _) => query.matches(keys)
  })

  lazy val duration = subintervals.values.fold(sum.zero)(sum.sum)

}


case class Query[KEY](mustBe: Set[KEY], cantBe: Set[KEY] = Set.empty[KEY], unionWith: Option[Query[KEY]] = None) {

  if (mustBe.intersect(cantBe).nonEmpty)
    throw new IllegalStateException(s"cannot require a key that has to be absent at the same time: ${mustBe.intersect(cantBe)}")

  def matches(activeKeys: Set[KEY]): Boolean =
    (mustBe.forall(activeKeys.contains(_)) &&
      !cantBe.exists(activeKeys.contains(_))) ||
      (if (unionWith.isDefined) unionWith.get.matches(activeKeys) else false)

  def and(key: KEY) = this.copy(mustBe = mustBe + key)

  def andNot(key: KEY) = this.copy(cantBe = cantBe + key)

  def or(unionWith: Query[KEY]): Query[KEY] =
    this.copy(unionWith = Some(
      if (this.unionWith.isDefined)
        unionWith.or(this.unionWith.get)
      else
        unionWith
    ))

  def mentionedKeys: Set[KEY] = mustBe ++ cantBe ++ (if (unionWith.isDefined) unionWith.get.mentionedKeys else Set[KEY]())

  override def toString: String = {
    (mustBe.map("+" + _) ++ cantBe.map("-" + _)).mkString(" & ") +
      (if (unionWith.isDefined) " | " + unionWith.get else "")
  }

}

object Query {

  import scala.language.implicitConversions

  def empty[KEY]: Query[KEY] = Query[KEY](Set.empty[KEY])

  def key[KEY](key: KEY): Query[KEY] = Query[KEY](mustBe = Set(key))

  implicit class Any2Query[KEY](k: KEY) {
    def and(q: KEY) = Query.key(k) and q
    def andNot(q: KEY) = Query.key(k) andNot  q
    def or(q: Query[KEY]) = Query.key(k) or q
    def or(q: KEY) = Query.key(k) or Query.key(q)
  }

}

object IntersectingSubintervals {

  def of[KEY, BOUND, DURATION](intervalGroups: Map[KEY, Seq[Interval[BOUND]]])(implicit ord: Ordering[BOUND]) =
    SubintervalsCalculator.calculateSubintervals(intervalGroups)

}