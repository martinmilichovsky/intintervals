package intintersections

import java.time.temporal.ChronoUnit
import java.time._

trait ExtractDuration[T, L] { def length(start: T, end: T): L }
trait SumDuration[L] { def zero: L; def sum(a: L, b: L): L }

object ExtractDuration {
  implicit def numericExtract[T](implicit ev: Numeric[T]) = new ExtractDuration[T, T] {
    override def length(start: T, end: T): T = ev.minus(end, start)
  }
}

object SumDuration {
  implicit def numericSum[T](implicit ev: Numeric[T]) = new SumDuration[T] {
    override def sum(a: T, b: T): T = ev.plus(a, b)
    override def zero: T = ev.zero
  }
}

object Implicits {

  object LocalDateTimeDuration {

    implicit def timeExtract = new ExtractDuration[LocalDateTime, Duration] {
      override def length(start: LocalDateTime, end: LocalDateTime): Duration = Duration.between(start, end)
    }

    implicit def sumDuration = new SumDuration[Duration] {
      override def zero: Duration = Duration.ZERO
      override def sum(a: Duration, b: Duration): Duration = a plus b
    }

    implicit val timeOrd: Ordering[LocalDateTime] = Ordering.fromLessThan[LocalDateTime]((a, b) => a.compareTo(b)<0)

  }

  object LocalDateTimeMillis {

    implicit def timeExtract = new ExtractDuration[LocalDateTime, Long] {
      override def length(start: LocalDateTime, end: LocalDateTime): Long = ChronoUnit.MILLIS.between(start, end)
    }

    implicit def sumDuration = implicitly[SumDuration[Long]]

    implicit val timeOrd: Ordering[LocalDateTime] = Ordering.fromLessThan[LocalDateTime]((a, b) => a.compareTo(b)<0)

  }

  object LocalDatePeriod {

    implicit def timeExtract = new ExtractDuration[LocalDate, Period] {
      override def length(start: LocalDate, end: LocalDate): Period = Period.between(start, end)
    }

    implicit def sumDuration = new SumDuration[Period] {
      override def zero: Period = Period.ZERO
      override def sum(a: Period, b: Period): Period = a plus b
    }

    implicit val dateOrd: Ordering[LocalDate] = Ordering.fromLessThan[LocalDate]((a, b) => a.compareTo(b)<0)

  }

  object LocalDateDays {

    implicit def timeExtract = new ExtractDuration[LocalDate, Long] {
      override def length(start: LocalDate, end: LocalDate): Long = ChronoUnit.DAYS.between(start, end)
    }

    implicit def sumDuration = implicitly[SumDuration[Long]]

    implicit val dateOrd: Ordering[LocalDate] = Ordering.fromLessThan[LocalDate]((a, b) => a.compareTo(b)<0)

  }

}