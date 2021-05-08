package core

object PlayerTurns extends Enumeration {
  type PlayerTurn = Value

  val South, North = Value

  def nextPlayer: PlayerTurn => PlayerTurn = _ match {
    case South => North
    case North => South
  }
}
