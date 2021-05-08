package utils

import com.typesafe.config.ConfigFactory
import core.PlayerTurns._

object Config {
  private val config = ConfigFactory.load()
  private val gameConfig = config.getConfig("game")

  val boardSize: Int = gameConfig.getInt("size")
  val firstPlayer: PlayerTurn = if (gameConfig.getBoolean("south-first")) South else North
  val maxDepth: Int = gameConfig.getInt("max-depth")
}
