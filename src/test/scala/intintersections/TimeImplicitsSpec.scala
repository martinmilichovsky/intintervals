package intintersections

import java.time._
import java.time.temporal.{ChronoUnit, Temporal, TemporalField, WeekFields}

import org.scalatest.FunSuite

class TimeImplicitsSpec extends FunSuite {

  import Query._
  test("time implicit") {
    import Implicits.LocalDateTimeDuration._
    val mondayMidnight = LocalDateTime.of(2016, 1, 4, 0, 0, 0) // as a reference
    val calendar = (1 to 365).flatMap {
      dayOfYear => {
        val day = mondayMidnight.withDayOfYear(dayOfYear) // every day of year
        val nightTimeStart = day.withHour(23) // 11PM
        val nightTimeEnd = nightTimeStart.plusDays(1).withHour(6) // to 6AM of the next day
        Seq(
          ("night", Interval(nightTimeStart, nightTimeEnd)), // nighttime
          (day.getDayOfWeek.toString, Interval(day, day.plusDays(1))) // day of week - e.g. "MONDAY"
        )
      }
    }

    // sample attendance of employee
    val timeAtWork = Seq(
      Interval(mondayMidnight.plusHours(8), mondayMidnight.plusHours(16).plusMinutes(30)), // 8am to 4:30pm at work on Monday
      Interval(mondayMidnight.plusDays(1).plusHours(8), mondayMidnight.plusDays(1).plusHours(16)), // and Tuesday
      Interval(mondayMidnight.plusDays(4).plusHours(16), mondayMidnight.plusDays(4).plusHours(16+24)) // 24 hours in work since Friday 4pm
    )

    val input = calendar
      .groupBy(_._1)
      .mapValues(_.map(_._2)) ++ Map("work" -> timeAtWork)

    val intervals = IntersectingSubintervals.of(input)

    val workingIntervals = intervals.filter("work")

    assert(workingIntervals.duration == Duration.ofHours(40).plusMinutes(30))

    assert(workingIntervals.filter("SATURDAY").duration == Duration.ofHours(16),
      "16 hours of work on Sat (started 24h shift on Fri at 16:00)")

    val regularHoursWorked = workingIntervals.filter(Query.empty andNot "night" andNot "SATURDAY" andNot "SUNDAY").duration
    val weekendNightHoursWorked = workingIntervals.filter("SATURDAY" and "night" or ("SUNDAY" and "night")).duration
    val nightHoursWorked = workingIntervals.filter(Query.empty and "night" andNot "SATURDAY" andNot "SUNDAY").duration
    val weekendHoursWorked = workingIntervals.filter("SATURDAY" andNot "night" or ("SUNDAY" andNot "night")).duration

    assert(regularHoursWorked == Duration.ofHours(23).plusMinutes(30)) // 8.5 Mon + 8 Tue + 7 Fri (4pm-11pm)
    assert(weekendNightHoursWorked == Duration.ofHours(6)) // Sat 0-6AM
    assert(nightHoursWorked == Duration.ofHours(1)) // Fri 11pm-12PM
    assert(weekendHoursWorked == Duration.ofHours(10)) // Sat 6am-4pm

  }

  test("date implicit") {

    import Implicits.LocalDatePeriod._

    for (offset <- 0 to 1000) {
      val t0 = LocalDate.now().withDayOfMonth(1).withMonth(1).plusDays(offset)
      val input = Map(
        "last 365 days" -> Seq(Interval(t0.minusYears(1), t0)),
        "this year" -> Seq(Interval(t0.withDayOfMonth(1).withMonth(1), t0.withMonth(12).withDayOfMonth(31))),
        "sundays +-2yrs" -> {
          var date = t0.withDayOfMonth(1).withMonth(1).minusYears(2)
          while (date.getDayOfWeek != DayOfWeek.SUNDAY)
            date = date.plusDays(1)
          Iterator.continually {
            val d = date
            date = date.plusDays(7)
            d
          }.takeWhile(_.getYear <= t0.getYear + 2).map(sunday => Interval(sunday, sunday.plusDays(1))).toSeq
        }
      )

      val ints = IntersectingSubintervals.of(input)

      {
        import Implicits.LocalDateDays._
        assert(t0.getDayOfYear - 1 ===
          ints.filter("last 365 days" and "this year").duration)
      }

      val weeksInYear = ChronoUnit.WEEKS.between(t0.withMonth(1).withDayOfMonth(1), t0).toInt
      assert((weeksInYear to weeksInYear + 1) contains
        ints.filter("sundays +-2yrs" and "this year" and "last 365 days").duration.getDays,
        s"-- number of sundays this year so far is number of weeks this year so far, or one less; t0 = $t0"
      )
    }

  }

}
