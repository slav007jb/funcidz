package tracker

object CsvParser:

  private val Columns = 5

  private val LogHoursMin = BigDecimal("0.25") // границы полей по варику 
  private val LogHoursMax = BigDecimal("8.00")
  private val FixHoursMin = BigDecimal("0.25")
  private val FixHoursMax = BigDecimal("2.50")
  private val PaymentMin  = BigDecimal("1200.00")
  private val PaymentMax  = BigDecimal("45000.00")

  private def field[A](line: Int, name: String, r: Either[FieldError, A]): Either[ImportError, A] =
    r.left.map(err => ImportError.BadField(line, name, err))

  def parseLine(lineNo: Int, line: String): Either[ImportError, Event] =
    val cells = line.split(",", -1).toList
    if cells.length != Columns then
      Left(ImportError.MalformedRow(lineNo, line, Columns, cells.length))
    else
      cells match
        case "LOG_TIME" :: p1 :: p2 :: p3 :: _ :: Nil =>
          for
            project <- field(lineNo, "project", Project.fromString(p1))
            task    <- field(lineNo, "task", Task.fromString(p2))
            hours   <- field(lineNo, "hours", Hours.parse(p3, LogHoursMin, LogHoursMax))
          yield Event.LogTime(project, task, hours)

        case "INVOICE" :: p1 :: _ :: _ :: _ :: Nil =>
          for
            project <- field(lineNo, "project", Project.fromString(p1))
          yield Event.Invoice(project)

        case "PAYMENT" :: p1 :: p2 :: _ :: _ :: Nil =>
          for
            project <- field(lineNo, "project", Project.fromString(p1))
            amount  <- field(lineNo, "amount", Money.parse(p2, PaymentMin, PaymentMax))
          yield Event.Payment(project, amount)

        case "UNPAID_FIX" :: p1 :: p2 :: _ :: _ :: Nil =>
          for
            project <- field(lineNo, "project", Project.fromString(p1))
            hours   <- field(lineNo, "hours", Hours.parse(p2, FixHoursMin, FixHoursMax))
          yield Event.UnpaidFix(project, hours)

        case code :: _ =>
          Left(ImportError.UnknownEvent(lineNo, code))

        case Nil =>
          Left(ImportError.UnknownEvent(lineNo, ""))

  def parseBatch(lines: List[String], firstLineNo: Int = 1): (List[ImportError], List[Event]) =
    lines.zipWithIndex.partitionMap { case (line, idx) => parseLine(firstLineNo + idx, line) }