package com.htvu.gokata

import org.scalatest.FlatSpec
import org.scalatest.TryValues._

class SelfCheckSpec extends FlatSpec {
  val board = Board(Array(
    "ww.",
    "bb."
  ))

  val check = new SelfCheck with BoardTraversable{}

  "nonEmptyCheck" should "only allow putting in non-empty cell" in {
    assert(check.nonEmptyCheck(board, 0, 0).failure.exception.getMessage === "Cell already occupied")
    assert(check.nonEmptyCheck(board, 1, 0).failure.exception.getMessage === "Cell already occupied")
    assert(check.nonEmptyCheck(board, 1, 2).success.value === board)
  }

  "captureOpponentCheck" should "remove opponents chess if their liberty is zero" in {
    val afterCaptured = Board(Array(
      "..b",
      "bb."
    ))
    assert(check.captureOpponentCheck(board, 0, 2, Black).success.value.cells === afterCaptured.cells)
  }
}

class NewBoardCheckSpec extends FlatSpec {
  val board = Board(Array(
    ".b",
    "b.",
    ".."
  ))

  val check = new NewBoardCheck with BoardTraversable {}

  "noSelfCaptureCheck" should "not allow new cell to be immediately captured" in {
    assert(check.noSelfCaptureCheck(board, 0, 0, White).failure.exception.getMessage === "Is self-captured")
  }
}

class BoardTraversalSpec extends FlatSpec {
  val board = Board(Array(
    "wbww.",
    "wwbbb",
    "bb..."
  ))

  val traversable = new BoardTraversable {}

  "getCapturedCells" should "return all captured cells starting from(i,j)" in {
    val captured = Set((0, 0), (1, 0), (1, 1))
    assert(traversable.getCapturedCells(board, 1, 1).map(_.toSet).success.value === captured)
  }

  it should "return empty if no cell are captured" in {
    assert(traversable.getCapturedCells(board, 0, 2).success.value  === Board.EmptyPosition)
  }

  it should "return Failure if something went wrong" in {

  }
}

class BoardSpec extends FlatSpec {
  "move" should "calculate liberty after removing captured opponent if any" in {
    val board = Board(Array(
      ".b",
      "bw"
    ))
    val expected = Board(Array(
      "w.",
      ".w"
    ))
    assert(board.move(Board(5, 5), 0, 0, White).success.value.cells === expected.cells)
  }

  it should "not allow move that result in the same state as the previous move" in {
    val board = Board(Array(
      ".wb.",
      "w.wb",
      ".wb."
    ))
    val before = Board(Array(
    ".wb.",
    "wb.b",
    ".wb."
  ))
    assert(board.move(before, 1, 1, Black).failure.exception.getMessage === "Infinite loop")
  }
}