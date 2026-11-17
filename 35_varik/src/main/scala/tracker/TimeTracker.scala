package tracker

/** Події системи (варіант 35). */
enum Event:
  /** i. Запуск/зупинка таймера: проєкт, опис, тривалість у годинах. */
  case TimeLogged(project: String, description: String, hours: BigDecimal)
  /** ii. Виставлення рахунку: години закриті до оплати. */
  case Invoiced(project: String, hours: BigDecimal)
  /** iii. Отримання оплати за погодинною ставкою. */
  case PaymentReceived(project: String, hours: BigDecimal, rate: BigDecimal)
  /** iv. Неоплачуваний овертайм / виправлення власного бага. */
  case Overtime(project: String, description: String, hours: BigDecimal)

/** Підсумки по одному проєкту. */
final case class ProjectStats(
    logged: BigDecimal = 0,   // оплачувані години
    invoiced: BigDecimal = 0, // години, включені в рахунок
    paid: BigDecimal = 0,     // години, за які прийшла оплата
    overtime: BigDecimal = 0, // неоплачувані години
    earned: BigDecimal = 0    // зароблені гроші
):
  def unpaidHours: BigDecimal = logged - paid
  def totalWorkedHours: BigDecimal = logged + overtime

/** Поточний стан системи + аналітика. */
final case class State(projects: Map[String, ProjectStats] = Map.empty):

  /** Загальна кількість ще неоплачених годин. */
  def totalUnpaidHours: BigDecimal = projects.values.map(_.unpaidHours).sum

  /** Загальна сума зароблених коштів за проєктом. */
  def earnedFor(project: String): BigDecimal = stats(project).earned

  /** Проєкт, на який витрачено найбільше робочого часу (оплачувані + овертайм). */
  def busiestProject: Option[String] =
    projects.maxByOption { case (_, s) => s.totalWorkedHours }.map(_._1)

  def stats(project: String): ProjectStats = projects.getOrElse(project, ProjectStats())

  def update(project: String)(f: ProjectStats => ProjectStats): State =
    copy(projects = projects.updated(project, f(stats(project))))

object TimeTracker:

  private val MaxHours: BigDecimal = 24

  /** Тривалість запису: не менше 1 хвилини (1/60 год) і не більше 24 годин. */
  private def validDuration(hours: BigDecimal): Boolean =
    hours * 60 >= 1 && hours <= MaxHours

  /** Застосувати одну подію: або новий стан, або повідомлення про помилку. */
  def apply(state: State, event: Event): Either[String, State] = event match
    case Event.TimeLogged(project, _, hours) =>
      if !validDuration(hours) then Left("Тривалість запису має бути від 1 хвилини до 24 годин")
      else Right(state.update(project)(s => s.copy(logged = s.logged + hours)))

    case Event.Overtime(project, _, hours) =>
      if !validDuration(hours) then Left("Тривалість запису має бути від 1 хвилини до 24 годин")
      else Right(state.update(project)(s => s.copy(overtime = s.overtime + hours)))

    case Event.Invoiced(project, hours) =>
      val s = state.stats(project)
      if hours <= 0 then Left("Кількість годин у рахунку має бути додатною")
      else if hours > s.logged - s.invoiced then Left("У рахунок не можна включити більше годин, ніж залоговано")
      else Right(state.update(project)(_.copy(invoiced = s.invoiced + hours)))

    case Event.PaymentReceived(project, hours, rate) =>
      val s = state.stats(project)
      if hours <= 0 || rate <= 0 then Left("Години та ставка мають бути додатними")
      else if hours > s.invoiced - s.paid then Left("Оплата перевищує кількість виставлених до оплати годин")
      else Right(state.update(project)(_.copy(paid = s.paid + hours, earned = s.earned + hours * rate)))

  /** Обробити послідовність подій; зупиняється на першій помилці. */
  def run(events: List[Event]): Either[String, State] =
    events.foldLeft[Either[String, State]](Right(State()))((acc, e) => acc.flatMap(apply(_, e)))
