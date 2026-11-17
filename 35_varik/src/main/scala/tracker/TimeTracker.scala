package tracker

enum Event:
  case TimeLogged(project: String, description: String, hours: BigDecimal) // запуск/остановка таймера, инфа про проект, описание, длительность в часах
  case Invoiced(project: String, hours: BigDecimal) // выставление счета часы закрыты еще до оплаты
  case PaymentReceived(project: String, hours: BigDecimal, rate: BigDecimal) // получение оплаты за часовой ставкой
  case Overtime(project: String, description: String, hours: BigDecimal) // неоплачеваемый овертайм

final case class ProjectStats(
    logged: BigDecimal = 0,   // оплаченные часы
    invoiced: BigDecimal = 0, // часы включенные в счет
    paid: BigDecimal = 0,     // часы за которые прошла оплата
    overtime: BigDecimal = 0, // неоплаченные часы
    earned: BigDecimal = 0    // заработанные деньги
):
  def unpaidHours: BigDecimal = logged - paid
  def totalWorkedHours: BigDecimal = logged + overtime

final case class State(projects: Map[String, ProjectStats] = Map.empty): // текущее состояние системы + аналитика
  def totalUnpaidHours: BigDecimal = projects.values.map(_.unpaidHours).sum // суммарное кол-во не оплаченных часов
  def earnedFor(project: String): BigDecimal = stats(project).earned   // общая сумма заработанных денег за проект
  def busiestProject: Option[String] =  // Проект на который потрачено много времени т.е оплаченные + овертайм
    projects.maxByOption { case (_, s) => s.totalWorkedHours }.map(_._1)

  def stats(project: String): ProjectStats = projects.getOrElse(project, ProjectStats())

  def update(project: String)(f: ProjectStats => ProjectStats): State =
    copy(projects = projects.updated(project, f(stats(project))))

object TimeTracker:

  private val MaxHours: BigDecimal = 24

  private def validDuration(hours: BigDecimal): Boolean = // длительность записи не меньше минуты не больше суток
    hours * 60 >= 1 && hours <= MaxHours

  def apply(state: State, event: Event): Either[String, State] = event match // задействовать либо одно действие либо новое состояние либо сообщение об ошибке
    case Event.TimeLogged(project, _, hours) => // запуск таймера
      if !validDuration(hours) then Left("Тривалість запису має бути від 1 хвилини до 24 годин")
      else Right(state.update(project)(s => s.copy(logged = s.logged + hours)))

    case Event.Overtime(project, _, hours) => // неоплачеваемый овертайм
      if !validDuration(hours) then Left("Тривалість запису має бути від 1 хвилини до 24 годин")
      else Right(state.update(project)(s => s.copy(overtime = s.overtime + hours)))

    case Event.Invoiced(project, hours) => // часы закрытые до оплаты
      val s = state.stats(project)
      if hours <= 0 then Left("Кількість годин у рахунку має бути додатною")
      else if hours > s.logged - s.invoiced then Left("У рахунок не можна включити більше годин, ніж залоговано")
      else Right(state.update(project)(_.copy(invoiced = s.invoiced + hours)))

    case Event.PaymentReceived(project, hours, rate) => // получение оплаты за часовой ставкой
      val s = state.stats(project)
      if hours <= 0 || rate <= 0 then Left("Години та ставка мають бути додатними")
      else if hours > s.invoiced - s.paid then Left("Оплата перевищує кількість виставлених до оплати годин")
      else Right(state.update(project)(_.copy(paid = s.paid + hours, earned = s.earned + hours * rate)))

  def run(events: List[Event]): Either[String, State] = // обработка полсдеовательности действий, останавливается в случае первой-же оишкбки
    events.foldLeft[Either[String, State]](Right(State()))((acc, e) => acc.flatMap(apply(_, e)))
