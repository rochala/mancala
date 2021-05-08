package core

import core.PlayerTurns._
import utils.Config.maxDepth

trait Player {
  def getMove(boardState: State): Int
}

class MinimaxPlayer(
    maxDepth: Int = maxDepth,
    evaluate: State => Int = State.evaluate
) extends Player {
  def getMove(boardState: State): Int = {
    def helper(depth: Int, boardState: State, move: Int): (Int, Int) = {
      if (depth == maxDepth || boardState.avaliableMoves.isEmpty)
        return (move, evaluate(boardState))

      boardState.turn match {
        case South => {
          boardState.avaliableMoves
            .map(localMove =>
              helper(depth + 1, boardState.makeMove(localMove).get, move)
            )
            .maxBy(_._2)
        }
        case North => {
          boardState.avaliableMoves
            .map(localMove =>
              helper(depth + 1, boardState.makeMove(localMove).get, move)
            )
            .minBy(_._2)
        }
      }
    }

    val moves = boardState.avaliableMoves
      .map(localMove =>
        helper(1, boardState.makeMove(localMove).get, localMove)
      )

    val move = boardState.turn match {
      case South => moves.maxBy(_._2)._1
      case North => moves.minBy(_._2)._1
    }
    move
  }
}

class AlphaBetaPlayer(
    maxDepth: Int = maxDepth,
    evaluate: State => Int = State.evaluate
) extends Player {
  def getMove(boardState: State): Int = {
    def helper(
        depth: Int,
        boardState: State,
        move: Int,
        alpha: Int,
        beta: Int
    ): (Int, Int) = {
      if (depth == maxDepth || boardState.avaliableMoves.isEmpty)
        return (move, evaluate(boardState))

      var newAlpha = alpha
      var newBeta = beta
      boardState.turn match {
        case South => {
          var value = (move, Int.MinValue)
          boardState.avaliableMoves
            .map { localMove =>
              value = List(
                value,
                helper(
                  depth + 1,
                  boardState.makeMove(localMove).get,
                  move,
                  newAlpha,
                  newBeta
                )
              ).maxBy(_._2)
              newAlpha = List(alpha, value._2).max
              if (newAlpha >= newBeta) {
                return value
              }
              value
            }
            .maxBy(_._2)
        }
        case North => {
          var value = (move, Int.MaxValue)
          boardState.avaliableMoves
            .map { localMove =>
              value = List(
                value,
                helper(
                  depth + 1,
                  boardState.makeMove(localMove).get,
                  move,
                  newAlpha,
                  newBeta
                )
              ).minBy(_._2)
              newBeta = List(newBeta, value._2).min
              if (newBeta <= newAlpha) {
                return value
              }
              value
            }
            .minBy(_._2)
        }
      }
    }

    val moves = boardState
      .avaliableMoves()
      .map(localMove =>
        helper(
          1,
          boardState.makeMove(localMove).get,
          localMove,
          Int.MinValue,
          Int.MaxValue
        )
      )
    val move = boardState.turn match {
      case South => moves.maxBy(_._2)._1
      case North => moves.minBy(_._2)._1
    }
    move
  }
}

class HumanPlayer extends Player {
  def getMove(boardState: State): Int = {
    print("Enter your move: ")
    var move = scala.io.StdIn.readInt()

    while (!boardState.avaliableMoves.contains(move)) {
      print("\nIncorrect move try again: ")
      move = scala.io.StdIn.readInt()
    }
    move
  }
}
