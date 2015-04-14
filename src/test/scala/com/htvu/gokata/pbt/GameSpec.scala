package com.htvu.gokata.pbt

import com.htvu.gokata._
import org.scalacheck.{Prop, Gen}
import org.scalacheck.commands.Commands
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

import scala.util.{Failure, Try, Success}


case class Game(var board: Board) {
  var prev = Board(board.nrows, board.ncols)

  def move(i: Int, j: Int, c: Cell): Try[Board] = {
    val current = board
    board.move(prev, i, j, c) match {
      case Success(newBoard) => {
        prev = current
        board = newBoard
        Success(newBoard)
      }
      case e: Failure[Board] => e
    }
  }
}

object GameSpecfication extends Commands {
  override type State = Board

  override type Sut = Game

  abstract class MoveCommand(i: Int, j: Int, c: Cell) extends Command {
    type Result = Try[Board]

    def run(sut: Sut): Result = {
        sut.move(i, j, c)
    }
  }

  case class NonEmptyMove(i: Int, j: Int, c: Cell) extends MoveCommand(i: Int, j: Int, c: Cell) {
    def preCondition(state: State): Boolean = state.cells.exists(_.exists(!_.isEmpty))

    def nextState(state: State): State = state

    def postCondition(state: State, result: Try[Result]): Prop = result match {
      case Success(res) => res match {
        case Failure(_) => true
        case _ => false
      }
      case _ => false
    }
  }

  case class NormalMove(i: Int, j: Int, c: Cell) extends MoveCommand(i: Int, j: Int, c: Cell) {
    def preCondition(state: State): Boolean = state.cells.exists(_.exists(_.isEmpty))

    def postCondition(state: State, result: Try[Result]): Prop = result match {
      case Success(result) => result match {
        case Success(newBoard) => state.cells.flatten.count(_.isEmpty) == newBoard.cells.flatten.count(_.isEmpty) + 1
        case _ => false
      }
      case _ => false
    }

    def nextState(state: State): State = Board(state.cells, i, j, c)
  }

  def genInitialState: Gen[GameSpecfication.State] = Gen.const(2) map (n => Board(n, n))

  def newSut(state: GameSpecfication.State): Sut = new Game(state)

  def genCommand(state: GameSpecfication.State): Gen[GameSpecfication.Command] = for (
    i <- Gen.choose(0, state.nrows-1);
    j <- Gen.choose(0, state.ncols-1);
    c <- Gen.oneOf(Black, White)
  ) yield if (state.cells(i)(j).isEmpty) NormalMove(i, j, c) else NonEmptyMove(i, j, c)

  def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = true

  def destroySut(sut: Sut): Unit = {}

  def initialPreCondition(state: State): Boolean = true
}

class GameSpec extends PropSpec with Checkers {
  property("Game Specification") {
    check(GameSpecfication.property())
  }
}
