package tracker

import scala.io.Source
import scala.util.Using

@main def importCsv(path: String = "src/test/resources/variant_35_freelance_time.csv"): Unit =
  val lines = Using.resource(Source.fromFile(path, "UTF-8"))(_.getLines().toList)

  val (errors, events) = CsvParser.parseBatch(lines.drop(1), firstLineNo = 2)
  val (state, rejected) = TimeTracker.replay(events)

  def importKind(e: ImportError): String = e match
    case _: ImportError.MalformedRow           => "MalformedRow"
    case _: ImportError.UnknownEvent           => "UnknownEvent"
    case ImportError.BadField(_, name, cause)  => s"BadField($name: ${cause.productPrefix})"

  println(s"Файл: $path")
  println(s"Рядків даних: ${lines.size - 1}")
  println(s"Валідних подій: ${events.size}")
  println(s"Помилок парсингу: ${errors.size}")
  errors.groupBy(importKind).toList.sortBy(_._1).foreach { case (k, v) => println(s"  $k: ${v.size}") }
  println(s"Відхилено бізнес-правилами: ${rejected.size}")
  rejected.groupBy(_.productPrefix).toList.sortBy(_._1).foreach { case (k, v) => println(s"  $k: ${v.size}") }
  println()
  println(s"Загалом неоплачених годин: ${state.totalUnpaidHours}")
  println(s"Загалом зароблено: ${state.totalEarned}")
  println(s"Найбільш завантажений проєкт: ${state.busiestProject.map(_.value).getOrElse("-")}")
  println("Зароблено по проєктах:")
  state.projects.toList.sortBy(_._1.value).foreach { case (p, s) => println(s"  ${p.value}: ${s.earned}") }