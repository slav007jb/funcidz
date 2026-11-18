package tracker

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using

class TimeTrackerSpec extends AnyFunSuite:

  private def proj(s: String): Project = Project.fromString(s).toOption.get
  private def task(s: String): Task    = Task.fromString(s).toOption.get
  private def hrs(s: String): Hours    = Hours.parse(s).toOption.get

  test("Hours - від 1 хвилини до 24 годин") {
    assert(Hours.parse("0.0167").isRight) // ровно 1 минута
    assert(Hours.parse("24").isRight)     // ровно сутки
    assert(Hours.parse("0.008").isLeft)   // 30 сек запрещено
    assert(Hours.parse("24.5").isLeft)    // больше суток тож запрещено
    assert(Hours.parse("abc") == Left(FieldError.InvalidNumber("abc")))
    assert(Hours.parse("-1") == Left(FieldError.NotPositive("-1")))
  }

  test("parseLine - валідні рядки чотирьох типів подій") { // парсер ряда
    assert(CsvParser.parseLine(1, "LOG_TIME,PRJ1,fix-bug,2.50,") ==
      Right(Event.LogTime(proj("PRJ1"), task("fix-bug"), hrs("2.50"))))
    assert(CsvParser.parseLine(2, "INVOICE,PRJ1,,,") == Right(Event.Invoice(proj("PRJ1"))))
    assert(CsvParser.parseLine(3, "UNPAID_FIX,PRJ1,1.25,,") ==
      Right(Event.UnpaidFix(proj("PRJ1"), hrs("1.25"))))
    assert(CsvParser.parseLine(4, "PAYMENT,PRJ1,5000.00,,").isRight)
  }

  test("parseLine - невідома подія і неправильна кількість колонок") {
    assert(CsvParser.parseLine(7, "NOT_AN_EVENT,741DX8YFUT,1.23,,") ==
      Left(ImportError.UnknownEvent(7, "NOT_AN_EVENT")))
    assert(CsvParser.parseLine(3, "LOG_TIME,A0FFKB3E5BND") ==
      Left(ImportError.MalformedRow(3, "LOG_TIME,A0FFKB3E5BND", 5, 2)))
    assert(CsvParser.parseLine(4, "INVOICE,A0FFKB3E5BND,,,,EXTRA") ==
      Left(ImportError.MalformedRow(4, "INVOICE,A0FFKB3E5BND,,,,EXTRA", 5, 6)))
  }

  test("parseLine - помилки полів (токен, число, діапазон)") {
    assert(CsvParser.parseLine(1, "LOG_TIME,PRJ1,t1,-100.00,") ==
      Left(ImportError.BadField(1, "hours", FieldError.NotPositive("-100.00"))))
    assert(CsvParser.parseLine(1, "PAYMENT,PRJ1,abc,,") ==
      Left(ImportError.BadField(1, "amount", FieldError.InvalidNumber("abc"))))
    assert(CsvParser.parseLine(1, "LOG_TIME,bad token,t1,1.00,") ==
      Left(ImportError.BadField(1, "project", FieldError.InvalidToken("bad token"))))
    CsvParser.parseLine(1, "LOG_TIME,PRJ1,t1,9.00,") match
      case Left(ImportError.BadField(_, "hours", FieldError.OutOfRange(_, _, _))) => succeed
      case other => fail(s"неочікуваний результат: $other")
  }

  test("запис 30 секунд з CSV відхиляється (заборонена операція)") {
    assert(CsvParser.parseLine(1, "LOG_TIME,PRJ1,t1,0.008,").isLeft)
  }

  test("parseBatch - розділяє помилки та валідні події, зберігає номери рядків") { // пакетная обработка
    val lines = List(
      "LOG_TIME,PRJ1,t1,1.00,",
      "NOT_AN_EVENT,PRJ1,1.23,,",
      "INVOICE,PRJ1,,,",
      "LOG_TIME,PRJ1"
    )
    val (errors, events) = CsvParser.parseBatch(lines, firstLineNo = 10)
    assert(events.size == 2)
    val errorLines = errors.map {
      case ImportError.UnknownEvent(l, _)      => l
      case ImportError.MalformedRow(l, _, _, _) => l
      case ImportError.BadField(l, _, _)       => l
    }
    assert(errorLines == List(11, 13))
  }

  test("сценарій - логування, рахунок, оплата, неоплачуваний овертайм, підсумки") {
    val lines = List(
      "LOG_TIME,ALPHA,t1,2.00,",
      "LOG_TIME,ALPHA,t2,3.00,",
      "LOG_TIME,BETA,t3,3.00,",
      "INVOICE,ALPHA,,,",
      "PAYMENT,ALPHA,5000.00,,",
      "UNPAID_FIX,BETA,2.50,,"
    )
    val (errors, events) = CsvParser.parseBatch(lines)
    assert(errors.isEmpty)

    val (state, rejected) = TimeTracker.replay(events)
    assert(rejected.isEmpty)
    assert(state.totalUnpaidHours == BigDecimal(3))
    assert(state.earnedFor(proj("ALPHA")) == BigDecimal(5000))
    assert(state.earnedFor(proj("BETA")) == BigDecimal(0))
    assert(state.busiestProject.contains(proj("BETA")))
  }

  test("оплата без виставленого рахунку відхиляється, стан не змінюється") {
    val (errors, events) = CsvParser.parseBatch(List(
      "LOG_TIME,ALPHA,t1,2.00,",
      "PAYMENT,ALPHA,5000.00,,"
    ))
    assert(errors.isEmpty)
    val (state, rejected) = TimeTracker.replay(events)
    assert(rejected == List(DomainError.NothingToPay(proj("ALPHA"))))
    assert(state.totalEarned == BigDecimal(0))
    assert(state.totalUnpaidHours == BigDecimal(2))
  }

  test("повторний рахунок без нових годин відхиляється") {
    val (_, events) = CsvParser.parseBatch(List(
      "LOG_TIME,ALPHA,t1,2.00,",
      "INVOICE,ALPHA,,,",
      "INVOICE,ALPHA,,,"
    ))
    val (_, rejected) = TimeTracker.replay(events)
    assert(rejected == List(DomainError.NothingToInvoice(proj("ALPHA"))))
  }


  test("фрагмент variant_35_freelance_time.csv: імпорт і підсумки") {
    val lines = Using.resource(Source.fromResource("variant_35_fragment.csv", getClass.getClassLoader))(_.getLines().toList)
    val (errors, events) = CsvParser.parseBatch(lines.drop(1), firstLineNo = 2)

    assert(events.size == 67)
    assert(errors.size == 7)
    assert(errors.size + events.size == lines.size - 1)

    val (state, rejected) = TimeTracker.replay(events)
    assert(rejected.size == 15)
    assert(state.totalEarned == BigDecimal("96360.81"))
    assert(state.busiestProject.map(_.value).contains("A0FFKB3E5BND"))
}