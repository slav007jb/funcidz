package runner.bots

import runner.*
import scala.util.Random

/**
 * Рефлексивний бот (детермінований, на чистих патерн-матчингах).
 * Аналізує найближчий зріз observation.upcoming.head.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero

    val safeLateral = hero.lane match // движения влево/вправо + защита от выхода за границы
      case Lane.Left   => List(Action.MoveRight) // с левой полосы можно онли вправо
      case Lane.Right  => List(Action.MoveLeft) // с правой полосы можно онли влево
      case Lane.Center => List(Action.MoveLeft, Action.MoveRight) // с центра можно и туда и туда

    def targetLane(a: Action): Option[Lane] = a match // полоса на которую попадем после движения влево/вправо
      case Action.MoveLeft  => hero.lane.left // экшн влево - смена на левую полосу
      case Action.MoveRight => hero.lane.right // экшн вправо - смена на правую полосу
      case _                => None // не боковое движение - полосы на которую меняться нету 

    observation.upcoming.headOption match
      case None => Action.KeepRunning
      case Some(slice) =>
        val low  = slice.hasObstacleAt(hero.lane, Height.Low) // есть ли препятствие внизу
        val mid  = slice.hasObstacleAt(hero.lane, Height.Mid) // есть ли посерединке
        val high = slice.hasObstacleAt(hero.lane, Height.High) // есть ли наверху

        (low, mid, high) match
          case (false, false, _)   => Action.KeepRunning // внизу и по середине нет препятствия и можно бежать
          case (false, true, _)    => Action.Duck // внизу нету, по серединке есть, тогда приседаем
          case (true, _, false)    => Action.Jump // внизу заблокировано, а сверху свободно, так что прыгаем
          case (true, _, true)     => // внизу и сверху препятствия 
            // после бокового движения герой бежит дальше, поэтому на текущей полосе должны быть свободны низ и середина
            safeLateral
              .find { a =>
                targetLane(a).exists { l =>
                  !slice.hasObstacleAt(l, Height.Low) && !slice.hasObstacleAt(l, Height.Mid)
                }
              }
              .getOrElse(Action.KeepRunning)
