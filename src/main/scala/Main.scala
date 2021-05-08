import core._
import core.PlayerTurns._

object Main extends App {

  val depths = List((1, 1), (2, 2), (3, 3), (5, 5), (7, 7), (8, 8), (9, 9))
  val diffDepths = List((1, 2), (2,3), (3,5), (5,7), (7, 9))

  val heuristics: List[State => Int] = List(State.evaluate, State.mostPointsSum, State.sumPlayerBoardEval)

  val game = new Game

  for (i <- 0 to 5) {
    for (depth <- depths) {
      // for (heuristic <- heuristics) {
        game.newGame()
        var string = ""
        var turns = game.play(new MinimaxPlayer(depth._1), new MinimaxPlayer(depth._2), randomMoveValue = Some(i), firstMoveRandom = true)
        var winnerHistory = game.turnHistory.filter(_._1 == game.winner)
        var avg = winnerHistory.foldLeft(0L)(_ + _._3) / game.turnHistory.length
        string = game.winner match {
          case South => s"$avg;${turns._1}"
          case North => s"$avg;${turns._2}"
        }
        game.newGame()
        turns = game.play(new AlphaBetaPlayer(depth._1), new AlphaBetaPlayer(depth._2), randomMoveValue = Some(i), firstMoveRandom = true)
        winnerHistory = game.turnHistory.filter(_._1 == game.winner)
        avg = winnerHistory.foldLeft(0L)(_ + _._3) / winnerHistory.length
        game.winner match {
          case South => println(s"$string;$avg;${turns._1}")
          case North => println(s"$string;$avg;${turns._2}")
        // }
      }
    }
  }

  // game.newGame
  // var turns = game.play(new MinimaxPlayer(4), new MinimaxPlayer(4))
  // game.turnHistory.map(x => x._1 match {
  //   case North => 12 - x._2
  //   case South => x._2
  // }).map(println)
  // println(turns)
  // val game1 = game.turnHistory
  // println(game1.foldLeft(0L)(_ + _._3) / game1.length)
  // game.newGame
  // turns = game.play(new AlphaBetaPlayer(10, State.sumPlayerBoardEval), new AlphaBetaPlayer(10, State.sumPlayerBoardEval))
  // val game2 = game.turnHistory
  // println(game2.foldLeft(0L)(_ + _._3) / game2.length)
  // println(turns)

  // assert(game1 == game.getTurnHistory())
  // Game.game(MinimaxPlayer, MinimaxPlayer)
  // Game.game(MinimaxPlayer, HumanPlayer)
  // Game.game(HumanPlayer, HumanPlayer)
  // 5 1 0 4 3 5 2 2 5 2 1 0 0 2 5 4 4 0 3 5
}
