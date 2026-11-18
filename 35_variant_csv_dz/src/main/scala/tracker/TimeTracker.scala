package tracker

import scala.util.Try

enum FieldError: // ошибка одного поля
  case InvalidToken(raw: String)  // строка не подходит под шаблон токена пробел, запятая, пусто
  case InvalidNumber(raw: String) // не число 
  case NotPositive(raw: String)   // число есть но оно 0 или отрицательное
  case OutOfRange(raw: String, min: BigDecimal, max: BigDecimal) // число вне допустимых границ

enum ImportError: // ошибка импорта ряда с CSV
  case MalformedRow(line: Int, raw: String, expectedCols: Int, actualCols: Int) // не 5 колонок
  case UnknownEvent(line: Int, code: String)                                    // неизвестный код события
  case BadField(line: Int, field: String, cause: FieldError)                    // плохое поле

enum DomainError: // нарушение бизнесправил
  case NothingToInvoice(project: Project) // выставлять в счет нечего все часы уже в счете
  case NothingToPay(project: Project)     // платить не за что нет выставленного и неоплаченного счета

private[tracker] object Parse:
  private val TokenPattern = java.util.regex.Pattern.compile("[A-Za-z0-9._-]{1,64}")

  def token(raw: String): Boolean = TokenPattern.matcher(raw).matches()

  def positiveDecimal(raw: String): Either[FieldError, BigDecimal] =
    Try(BigDecimal(raw.trim)).toOption match
      case None              => Left(FieldError.InvalidNumber(raw))
      case Some(v) if v <= 0 => Left(FieldError.NotPositive(raw))
      case Some(v)           => Right(v)

  def ranged(raw: String, min: BigDecimal, max: BigDecimal): Either[FieldError, BigDecimal] =
    positiveDecimal(raw).flatMap { v =>
      if v < min || v > max then Left(FieldError.OutOfRange(raw, min, max)) else Right(v)
    }
    
final case class Project private (value: String)
object Project:
  // единственный способ получить Project либо валидный токен либо ошибка
  def fromString(raw: String): Either[FieldError, Project] =
    if Parse.token(raw) then Right(new Project(raw)) else Left(FieldError.InvalidToken(raw))

final case class Task private (value: String)
object Task:
  def fromString(raw: String): Either[FieldError, Task] =
    if Parse.token(raw) then Right(new Task(raw)) else Left(FieldError.InvalidToken(raw))

final case class Hours private (value: BigDecimal) // часы записи от 1 минуты т.е 0.0167 часа до суток
object Hours:
  val Min: BigDecimal = BigDecimal("0.0167") // 1 минута в часах
  val Max: BigDecimal = BigDecimal(24)       // сутки

  def parse(raw: String, min: BigDecimal = Min, max: BigDecimal = Max): Either[FieldError, Hours] =
    Parse.ranged(raw, min.max(Min), max.min(Max)).map(v => new Hours(v))

final case class Money private (value: BigDecimal) // сумма денег всегда положительная и в границах
object Money:
  def parse(raw: String, min: BigDecimal, max: BigDecimal): Either[FieldError, Money] =
    Parse.ranged(raw, min, max).map(v => new Money(v))


enum Event: // четыре события системы из задания
  case LogTime(project: Project, task: Task, hours: Hours) // запись оплачиваемого времени по задаче
  case Invoice(project: Project)                           // счет клиенту закрывает все часы проекта
  case Payment(project: Project, amount: Money)            // получена оплата за выставленный счет
  case UnpaidFix(project: Project, hours: Hours)           // неоплачиваемые часы овертайм 


final case class ProjectStats( // итоги по одному проекту
    loggedHours: BigDecimal = 0,    // оплаченные часы
    invoicedHours: BigDecimal = 0,  // часы закрытые счетом
    paidHours: BigDecimal = 0,      // часы за которые получено оплату
    unpaidFixHours: BigDecimal = 0, // неоплаченванный овертайм
    earned: BigDecimal = 0          // заработанные деньги
)

final case class State(projects: Map[Project, ProjectStats] = Map.empty):
  // итоги проекта если проекта еще нет возвращаем пустые
  def stats(project: Project): ProjectStats = projects.getOrElse(project, ProjectStats())

  // создает новое состояние с измененными итогами одного проекта 
  def update(project: Project)(f: ProjectStats => ProjectStats): State =
    copy(projects = projects.updated(project, f(stats(project))))

  def totalUnpaidHours: BigDecimal = projects.values.map(s => s.loggedHours - s.paidHours).sum // суммарное кол-во еще не полаченных часов
  def earnedFor(project: Project): BigDecimal = stats(project).earned // сума заработенных денег за проект
  def totalEarned: BigDecimal = projects.values.map(_.earned).sum // сколько сумамрно заработано
  def busiestProject: Option[Project] = // Проект на который потрачено много времени т.е оплаченные + овертайм
    projects.maxByOption { case (_, s) => s.loggedHours + s.unpaidFixHours }.map(_._1) // None, если проектов нет

object TimeTracker:

  def apply(state: State, event: Event): Either[DomainError, State] = event match // задействовать один ивент к стану
    case Event.LogTime(p, _, h) => // запись времени просто прибавляем часы к проекту
      Right(state.update(p)(s => s.copy(loggedHours = s.loggedHours + h.value)))

    case Event.UnpaidFix(p, h) => // неоплачиваемые часы отдельный счетчик на оплату не влияет
      Right(state.update(p)(s => s.copy(unpaidFixHours = s.unpaidFixHours + h.value)))

    case Event.Invoice(p) => // счет
      val s = state.stats(p)
      // есть часы которые еще не попали в счет - закрываем все залогированные
      if s.loggedHours > s.invoicedHours then Right(state.update(p)(_.copy(invoicedHours = s.loggedHours)))
      else Left(DomainError.NothingToInvoice(p)) // иначе отказ состояние не меняется

    case Event.Payment(p, amount) => // оплата
      val s = state.stats(p)
      // есть выставленные но еще не оплаченные часы - считаем их оплаченными и добавляем деньги
      if s.invoicedHours > s.paidHours then
        Right(state.update(p)(_.copy(paidHours = s.invoicedHours, earned = s.earned + amount.value)))
      else Left(DomainError.NothingToPay(p)) // иначе отказ оплата без счета

  def replay(events: List[Event]): (State, List[DomainError]) =
    val (state, rejected) = events.foldLeft((State(), List.empty[DomainError])) {
      case ((st, rej), ev) =>
        apply(st, ev) match
          case Right(next) => (next, rej)       
          case Left(err)   => (st, err :: rej)  
    }
    (state, rejected.reverse)