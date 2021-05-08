package core

import core.PlayerTurns._
import utils.Config._
import scala.collection.mutable.ArraySeq

object State {
  def evaluate(state: State): Int = {
    var southPoints = state.board(boardSize)
    var northPoints = state.board.last
    if (state.isFinished().isDefined) {
      state.isFinished.get match {
        case South => southPoints = state.board.slice(0, boardSize + 1).sum
        case North => northPoints = state.board.slice(boardSize + 1, state.board.length).sum
      }
    }
    southPoints - northPoints
  }

  def sumPlayerBoardEval(state: State): Int = {
    state.turn match {
      case South => -state.board.slice(0, boardSize).sum
      case North => state.board.slice(boardSize + 1, state.board.length - 1).sum
    }
  }

  // def biggestCapturesEval(state: State): Int = {
  //   // val southPoints = state.board.slice(0, boardSize).reduce((acc, points) => if (points == 0) List(oppositeIndex(points), acc).max else acc)
  //   // val northPoints = state.board.slice(boardSize + 1, state.board.length - 1).reduce((acc, points) => if (points == 0) List(oppositeIndex(points), acc).max else acc)

  //   // southPoints - northPoints
  // }

  def mostPointsSum(state: State): Int = {
    var southPoints = state.board(boardSize)
    var northPoints = state.board.last
    state.turn match {
      case South => southPoints = state.board.slice(0, boardSize).sum + state.board(boardSize)
      case North => northPoints = state.board.slice(boardSize + 1, state.board.length - 1).sum + state.board(state.board.length - 1)
    }
    southPoints - northPoints
  }

  def oppositeIndex(index: Int): Int = {
    (boardSize * 2) - index
  }
}

case class State(board: ArraySeq[Int], turn: PlayerTurn) {
  def this() = {
    this(
      (1 to (boardSize + 1) * 2)
        .map(index => if (index % (boardSize + 1) == 0) 0 else 4)
        .toArray,
      firstPlayer
    )
  }


  def playerBoard(): ArraySeq[Int] = {
    turn match {
      case South => board.slice(0, boardSize)
      case North => board.slice(boardSize + 1, board.length - 1).reverse
    }
  }

  def avaliableMoves(): ArraySeq[Int] = {
    playerBoard().zipWithIndex.filter(_._1 > 0).map(_._2)
  }



  def finishGame(): State = {
      val southPoints = board.slice(0, boardSize).reduce(_ + _) + board(boardSize)
      val northPoints = board.slice(boardSize + 1, board.length - 1).reduce(_ + _) + board(board.length - 1)

      var newBoard = (1 to (boardSize + 1) * 2).map(_ => 0)
      newBoard = newBoard.updated(boardSize, southPoints)
      newBoard = newBoard.updated(board.length - 1, northPoints)

      State(newBoard.toArray, turn)
  }

  def printBoard() = {
    print("\t| ")
    board.slice(boardSize + 1, board.length - 1).reverse.map(x => print(x + " | "))
    print("\n")
    print("  ")
    print(board(board.length - 1))
    (0 to boardSize).map(x => print("     "))
    print(board(boardSize))
    print("\n")
    print("\t| ")
    board.slice(0, boardSize).map(x => print(x + " | "))
    print("\n")
    (0 to boardSize).map(x => print("------"))
    print("\n")
    print(" " + turn + "   ")
    (0 to boardSize - 1).map(x => print("[" + x + "] "))
    print("  to move")
    // print(" " + turn + "   ")
    // (0 to boardSize - 1).reverse.map(x => print("[" + x + "] "))
    // print("  to move")
    println()
    println()
  }

  def isFinished(): Option[PlayerTurn] = {
    if (board.slice(0, boardSize).forall(_ == 0) || board.slice(boardSize + 1, board.length - 1).forall(_ == 0)) {
      if (board(boardSize) > board.last) Some(South) else Some(North)
    } else None
  }

  def makeMove(move: Int): Option[State] = {
    val startIndex = turn match {
      case South => if (board(move) == 0) return None else move
      case North =>
        if (board(board.length - 2 - (move)) == 0) return None
        else board.length - 2 - (move)
    }

    val (playerWell, enemyWell) = turn match {
      case South => (boardSize, board.length - 1)
      case North => (board.length - 1, boardSize)
    }

    var pebbles = board(startIndex)

    var newBoard = board.updated(startIndex, 0)

    var actualIndex = startIndex + 1
    while (pebbles != 0) {
      if (actualIndex != enemyWell) {
        newBoard(actualIndex) += 1
        pebbles -= 1
      }
      if (actualIndex == newBoard.length - 1) {
        actualIndex = 0
      } else {
        actualIndex += 1
      }
    }

    actualIndex = if (actualIndex == 0) newBoard.length - 1 else actualIndex - 1

    //check if lands in proper half and hits

    if (actualIndex == playerWell) {
      return Some(State(newBoard, turn))
    }

    def capture(wellIndex: Int, actualIndex: Int) = {
      val enemyPoints = newBoard(State.oppositeIndex(actualIndex))
      if (enemyPoints != 0) {
        newBoard(actualIndex) = 0
        newBoard(State.oppositeIndex(actualIndex)) = 0
        newBoard(wellIndex) += enemyPoints + 1
      }
    }

    turn match {
      case North =>
        if (
          actualIndex > boardSize && actualIndex < board.length && newBoard(
            actualIndex
          ) == 1
        ) capture(newBoard.length - 1, actualIndex)
      case South =>
        if (
          actualIndex >= 0 && actualIndex < boardSize && newBoard(
            actualIndex
          ) == 1
        ) capture(boardSize, actualIndex)
    }

    return Some(State(newBoard, PlayerTurns.nextPlayer(turn)))
  }
}
