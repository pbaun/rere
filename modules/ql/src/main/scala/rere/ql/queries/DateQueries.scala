package rere.ql.queries

import rere.ql.options.Options
import rere.ql.options.all._
import rere.ql.ql2.Term.TermType
import rere.ql.types._

trait DateQueries {

  // now
  trait NowQuery extends ReqlTime

  implicit class NowOp(val r: ReqlR) {
    def now(): NowQuery = new NowQuery {
      val command = TermType.NOW
      val string = "now"
      val arguments = Nil
      val options = Options.empty
    }
  }

  // time
  trait TimeQuery extends ReqlTime

  implicit class TimeOnROp(val r: ReqlR) {
    def time(year: ReqlInteger,
             month: ReqlInteger,
             day: ReqlInteger,
             timezone: ReqlString): TimeQuery = new TimeQuery {
      val command = TermType.TIME
      val string = "time"
      val arguments = year :: month :: day :: timezone :: Nil
      val options = Options.empty
    }

    def time(year: ReqlInteger,
             month: ReqlInteger,
             day: ReqlInteger,
             hour: ReqlInteger,
             minute: ReqlInteger,
             second: ReqlFloat,
             timezone: ReqlString): TimeQuery = new TimeQuery {
      val command = TermType.TIME
      val string = "time"
      val arguments = year :: month :: day :: hour :: minute :: second :: timezone :: Nil
      val options = Options.empty
    }
  }

  // epoch_time
  trait EpochTimeQuery extends ReqlTime

  implicit class EpochTimeOnROp(val r: ReqlR) {
    def epochTime(seconds: ReqlNumber): EpochTimeQuery = new EpochTimeQuery {
      val command = TermType.EPOCH_TIME
      val string = "epoch_time"
      val arguments = seconds :: Nil
      val options = Options.empty
    }
  }

  // iso8601
  trait ISO8601Query extends ReqlTime
  //TODO: typesafe alternative to timezone offset string ?

  implicit class ISO8601OnROp(val r: ReqlR) {
    def ISO8601(
      timeString: ReqlString,
      defaultTimezoneOptions: DefaultTimezoneOptions = WithoutTimezone
    ): ISO8601Query = new ISO8601Query {
      val command = TermType.ISO8601
      val string = "iso8601"
      val arguments = timeString :: Nil
      val options = defaultTimezoneOptions
    }
  }

  // in_timezone
  trait InTimezoneQuery extends ReqlTime
  //TODO: typesafe alternative to timezone offset string ?

  implicit class InTimezoneOnTimeOp(val time: ReqlTime) {
    def inTimezone(timezone: ReqlString): InTimezoneQuery = new InTimezoneQuery {
      val command = TermType.IN_TIMEZONE
      val string = "in_timezone"
      val arguments = time :: timezone :: Nil
      val options = Options.empty
    }
  }

  // timezone
  trait TimezoneQuery extends ReqlString

  implicit class TimezoneOnTimeOp(val time: ReqlTime) {
    def timezone(): TimezoneQuery = new TimezoneQuery {
      val command = TermType.TIMEZONE
      val string = "timezone"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // during
  trait DuringQuery extends ReqlBoolean

  implicit class DuringOnTimeOp(val time: ReqlTime) {
    def during(
      startTime: ReqlTime,
      endTime: ReqlTime,
      boundsOptions: BoundsOptions = DefaultBounds
    ): DuringQuery = new DuringQuery {
      val command = TermType.DURING
      val string = "during"
      val arguments = time :: startTime :: endTime :: Nil
      val options = boundsOptions
    }
  }

  // date
  trait DateQuery extends ReqlTime

  implicit class DateOnTimeOp(val time: ReqlTime) {
    def date(): DateQuery = new DateQuery {
      val command = TermType.DATE
      val string = "date"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // time_of_day
  trait TimeOfDayQuery extends ReqlFloat

  implicit class TimeOfDayOnTimeOp(val time: ReqlTime) {
    def timeOfDay(): TimeOfDayQuery = new TimeOfDayQuery {
      val command = TermType.TIME_OF_DAY
      val string = "time_of_day"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // year
  trait YearQuery extends ReqlInteger

  implicit class YearOnTimeOp(val time: ReqlTime) {
    def year(): YearQuery = new YearQuery {
      val command = TermType.YEAR
      val string = "year"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // month
  trait MonthQuery extends ReqlInteger

  implicit class MonthOnTimeOp(val time: ReqlTime) {
    def month(): MonthQuery = new MonthQuery {
      val command = TermType.MONTH
      val string = "month"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // day
  trait DayQuery extends ReqlInteger

  implicit class DayOnTimeOp(val time: ReqlTime) {
    def day(): DayQuery = new DayQuery {
      val command = TermType.DAY
      val string = "day"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // day_of_week
  trait DayOfWeekQuery extends ReqlInteger

  implicit class DayOfWeekOnTimeOp(val time: ReqlTime) {
    def dayOfWeek(): DayOfWeekQuery = new DayOfWeekQuery {
      val command = TermType.DAY_OF_WEEK
      val string = "day_of_week"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // day_of_year
  trait DayOfYearQuery extends ReqlInteger

  implicit class DayOfYearOnTimeOp(val time: ReqlTime) {
    def dayOfYear(): DayOfYearQuery = new DayOfYearQuery {
      val command = TermType.DAY_OF_YEAR
      val string = "day_of_year"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // hours
  trait HoursQuery extends ReqlInteger

  implicit class HoursOnTimeOp(val time: ReqlTime) {
    def hours(): HoursQuery = new HoursQuery {
      val command = TermType.HOURS
      val string = "hours"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // minutes
  trait MinutesQuery extends ReqlInteger

  implicit class MinutesOnTimeOp(val time: ReqlTime) {
    def minutes(): MinutesQuery = new MinutesQuery {
      val command = TermType.MINUTES
      val string = "minutes"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // seconds
  trait SecondsQuery extends ReqlFloat

  implicit class SecondsOnTimeOp(val time: ReqlTime) {
    def seconds(): SecondsQuery = new SecondsQuery {
      val command = TermType.SECONDS
      val string = "seconds"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // to_iso8601
  trait ToISO8601Query extends ReqlString

  implicit class ToISO8601OnTimeOp(val time: ReqlTime) {
    def toISO8601(): ToISO8601Query = new ToISO8601Query {
      val command = TermType.TO_ISO8601
      val string = "to_iso8601"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // to_epoch_time
  trait ToEpochTimeQuery extends ReqlFloat

  implicit class ToEpochTimeOnTimeOp(val time: ReqlTime) {
    def toEpochTime(): ToEpochTimeQuery = new ToEpochTimeQuery {
      val command = TermType.TO_EPOCH_TIME
      val string = "to_epoch_time"
      val arguments = time :: Nil
      val options = Options.empty
    }
  }

  // monday, tuesday, wednesday, thursday, friday, saturday, sunday
  trait DayOfWeekConstant extends ReqlInteger {
    val arguments = Nil
    val options = Options.empty
  }

  implicit class DayOfWeekConstantOp(val r: ReqlR) {
    def monday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.MONDAY
      val string = "monday"
    }
    def tuesday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.TUESDAY
      val string = "tuesday"
    }
    def wednesday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.WEDNESDAY
      val string = "wednesday"
    }
    def thursday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.THURSDAY
      val string = "thursday"
    }
    def friday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.FRIDAY
      val string = "friday"
    }
    def saturday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.SATURDAY
      val string = "saturday"
    }
    def sunday: DayOfWeekConstant = new DayOfWeekConstant {
      val command = TermType.SUNDAY
      val string = "sunday"
    }
  }

  // january, february, march, april, may, june, july, august, september, october, november, december
  trait MonthConstant extends ReqlInteger {
    val arguments = Nil
    val options = Options.empty
  }

  implicit class MonthConstantOp(val r: ReqlR) {
    def january: MonthConstant = new MonthConstant {
      val command = TermType.JANUARY
      val string = "january"
    }
    def february: MonthConstant = new MonthConstant {
      val command = TermType.FEBRUARY
      val string = "february"
    }
    def march: MonthConstant = new MonthConstant {
      val command = TermType.MARCH
      val string = "march"
    }
    def april: MonthConstant = new MonthConstant {
      val command = TermType.APRIL
      val string = "april"
    }
    def may: MonthConstant = new MonthConstant {
      val command = TermType.MAY
      val string = "may"
    }
    def june: MonthConstant = new MonthConstant {
      val command = TermType.JUNE
      val string = "june"
    }
    def july: MonthConstant = new MonthConstant {
      val command = TermType.JULY
      val string = "july"
    }
    def august: MonthConstant = new MonthConstant {
      val command = TermType.AUGUST
      val string = "august"
    }
    def september: MonthConstant = new MonthConstant {
      val command = TermType.SEPTEMBER
      val string = "september"
    }
    def october: MonthConstant = new MonthConstant {
      val command = TermType.OCTOBER
      val string = "october"
    }
    def november: MonthConstant = new MonthConstant {
      val command = TermType.NOVEMBER
      val string = "november"
    }
    def december: MonthConstant = new MonthConstant {
      val command = TermType.DECEMBER
      val string = "december"
    }
  }

}
