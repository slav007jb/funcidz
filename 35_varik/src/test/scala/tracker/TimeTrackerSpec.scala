package tracker

import org.scalatest.funsuite.AnyFunSuite

class TimeTrackerSpec extends AnyFunSuite:

  test("сценарій: логування -> рахунок -> оплата -> овертайм -> підсумки") {
    val result = TimeTracker.run(List(
      Event.TimeLogged("Alpha", "Верстка", BigDecimal(10)), // 10 логируем
      Event.TimeLogged("Beta", "API", BigDecimal(5)), // 5 логируем
      Event.Invoiced("Alpha", BigDecimal(6)), // 6 часов еще в запасе
      Event.PaymentReceived("Alpha", BigDecimal(4), BigDecimal(25)), // оплачено 4 по 25
      Event.Overtime("Beta", "Виправлення власного бага", BigDecimal(8)) // овертайм 8 ч
    ))
    val state = result.toOption.get

    assert(state.totalUnpaidHours == BigDecimal(11)) // Alpha - 10 заказано, 4 оплачено = 6 разница
    assert(state.earnedFor("Alpha") == BigDecimal(100)) // 4 ч * 25 = 100
    assert(state.earnedFor("Beta") == BigDecimal(0)) // залогирвано но ниче не оплачено + овертайм 8 часов
    assert(state.busiestProject.contains("Beta"))
  }

  test("овертайм не впливає на неоплачені години й заробіток") {
    val state = TimeTracker.run(List(Event.Overtime("Alpha", "Баг", BigDecimal(3)))).toOption.get
    assert(state.totalUnpaidHours == BigDecimal(0))
    assert(state.earnedFor("Alpha") == BigDecimal(0))
  }

  test("порожній стан: найзавантаженішого проєкту немає") {
    assert(State().busiestProject.isEmpty)
  }

  test("межі тривалості: 1 хвилина і 24 години дозволені") {
    assert(TimeTracker.run(List(Event.TimeLogged("A", "x", BigDecimal(1) / 60))).isRight)
    assert(TimeTracker.run(List(Event.TimeLogged("A", "x", BigDecimal(24)))).isRight)
  }

  test("запис менше 1 хвилини відхиляється") {
    val r = TimeTracker.run(List(Event.TimeLogged("A", "x", BigDecimal("0.01"))))
    assert(r.isLeft)
  }

  test("запис понад 24 години відхиляється") {
    val r = TimeTracker.run(List(Event.TimeLogged("A", "x", BigDecimal("24.5"))))
    assert(r.isLeft)
  }

  test("овертайм також підпадає під обмеження тривалості") {
    assert(TimeTracker.run(List(Event.Overtime("A", "x", BigDecimal(25)))).isLeft)
  }

  test("не можна виставити рахунок на більше годин, ніж залоговано") {
    val r = TimeTracker.run(List(
      Event.TimeLogged("A", "x", BigDecimal(2)),
      Event.Invoiced("A", BigDecimal(3))
    ))
    assert(r.isLeft)
  }

  test("не можна отримати оплату за невиставлені години") {
    val r = TimeTracker.run(List(
      Event.TimeLogged("A", "x", BigDecimal(2)),
      Event.PaymentReceived("A", BigDecimal(2), BigDecimal(20))
    ))
    assert(r.isLeft)
  }
