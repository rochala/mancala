package core
import core.PlayerTurns._
import scala.util.Random
import core.State
import utils.Config

class Game(var gameState: State = new State()) {
  private var playedTurns: (Int, Int) = (0, 0)
  private var playedMoves: List[(PlayerTurn, Int, Long)] = List()
  var winner: PlayerTurn = South

  def newGame() = {
    gameState = new State()
    playedTurns = (0, 0)
    playedMoves = List()
  }

  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    // println("Elapsed time: " + (t1 - t0) + "ns")
    (result, t1 - t0)
  }

  def turnHistory: List[(PlayerTurn, Int, Long)] = playedMoves.reverse

  def play(
      southPlayer: Player,
      northPlayer: Player,
      firstMoveRandom: Boolean = false,
      randomMoveValue: Option[Int] = None,
      stop: Boolean = false,
      verbose: Boolean = false
  ): (Int, Int) = {
    def helper(): Unit = {
      gameState.isFinished() match {
        case Some(value) => {
          gameState = gameState.finishGame()
          winner = gameState.isFinished().get
          if (verbose) {
            println(s"$winner has won the game!")
            gameState.printBoard()
          }
          return
        }
        case None =>
      }

      if (verbose) gameState.printBoard()
      if (stop) scala.io.StdIn.readLine()

      val move = time(gameState.turn match {
        case South => southPlayer.getMove(gameState)
        case North => northPlayer.getMove(gameState)
      })

      if (verbose) println(s"${gameState.turn} player moved $move.")

      gameState.turn match {
        case South => playedTurns = (playedTurns._1 + 1, playedTurns._2)
        case North => playedTurns = (playedTurns._1, playedTurns._2 + 1)
      }

      playedMoves = (gameState.turn, move._1, move._2) :: playedMoves

      val newState = gameState.makeMove(move._1)

      gameState = newState match {
        case Some(value) => value
        case None        => throw new Exception
      }

      gameState = newState match {
        case Some(value) => value
        case None        => throw new Exception
      }

      helper()
    }

    if (firstMoveRandom) {
      val randomMove =
        if (randomMoveValue.isEmpty)
          Random.nextInt(gameState.avaliableMoves.length)
        else List(randomMoveValue.get, Config.boardSize).min

      if (verbose) {
        gameState.printBoard()
        println(s"First random move made was: $randomMove")
      }
      gameState =
        gameState.makeMove(gameState.avaliableMoves.apply(randomMove)).get
    }

    helper()
    playedTurns
  }
}
