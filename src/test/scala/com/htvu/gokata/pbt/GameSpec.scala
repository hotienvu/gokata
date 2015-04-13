package com.htvu.gokata.pbt

import com.htvu.gokata._
import org.scalacheck.{Prop, Gen}
import org.scalacheck.commands.Commands

import scala.util.{Failure, Try, Success}


case class Game(var board: Board) {
  var prev = Board(board.nrows, board.ncols)

  def move(i: Int, j: Int, c: Cell): Board = {
    val current = board
    board.move(prev, i, j, c) match {
      case Success(newBoard) => {
        prev = current
        board = newBoard
        board
      }
      case _ => { /* do nothing */ }
    }
    board
  }
}

object GameSpec extends Commands {
  override type State = Board

  override type Sut = Game

  trait MoveCommand extends Command {
    type Result = Board

    var move: Move = null

    def genMove(sut: Sut): Gen[Move]

    def run(sut: Sut): Result = {
      genMove(sut).sample match {
        case Some(Move(i, j, c)) => {
          move = Move(i, j, c)
          sut.move(i, j, c)
          sut.board
        }
        case _ => {
          throw new Exception("failed")
        }
      }
    }
  }

  case object NonEmptyMove extends MoveCommand {
    def preCondition(state: State): Boolean = state.cells.exists(_.exists(!_.isEmpty))

    def genMove(sut: Sut): Gen[Move] = {
      val board = sut.board
      val nonEmptyCells = board.cells.flatten.zipWithIndex.filter(!_._1.isEmpty).map { case (c, i) => (i / board.ncols, i % board.ncols)}
      for (
        (i, j) <- Gen.oneOf(nonEmptyCells);
        c <- Gen.oneOf(Black, White)
      ) yield Move(i, j, c)
    }

    def nextState(state: State): State = state

    def postCondition(state: State, result: Try[Result]): Prop = result match {
      case Success(newBoard) => state === newBoard
      case _ => false
    }
  }

  case object NormalMove extends MoveCommand {
    override def preCondition(state: State): Boolean = state.cells.exists(_.exists(_.isEmpty))

    override def postCondition(state: State, result: Try[Result]): Prop = result match {
      case Success(newBoard) => state.cells.count(_.isEmpty) + 1 == state.cells.count(_.isEmpty)
      case _ => false
    }

    def genMove(sut: Sut): Gen[Move] = {
      val board = sut.board
      val emptyCells = board.cells.flatten.zipWithIndex.filter(_._1.isEmpty).map { case (c, i) => (i / board.ncols, i % board.ncols)}
      for (
        (i, j) <- Gen.oneOf(emptyCells);
        c <- Gen.oneOf(Black, White)
      ) yield Move(i, j, c)
    }

    override def nextState(state: State): State = Board(state.cells, move.i, move.j, move.c)
  }

  def genInitialState: Gen[GameSpec.State] = Gen.choose(3, 10) map (n => Board(n, n))

  def newSut(state: GameSpec.State): Sut = new Game(state)

  def genCommand(state: GameSpec.State): Gen[GameSpec.Command] = Gen.oneOf(NormalMove, NonEmptyMove)

  def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = true

  def destroySut(sut: Sut): Unit = {}

  def initialPreCondition(state: State): Boolean = true
}
