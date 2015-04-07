import scala.collection.immutable.HashSet
import scala.util.{Failure, Success, Try}

object Board {
  def apply(numRows: Int, numCols: Int) = {
    val cells = Array.ofDim[Cell](numRows, numCols)
    for (xs <- cells; i <- 0 until numCols)
      xs(i) = Empty
    Board(cells)
  }

  def apply(_cells: Array[Array[Cell]], i: Int, j: Int, c: Cell): Board = {
    val cells = _cells.clone()
    cells(i)(j) = c
    Board(cells)
  }
}


trait BoardCheck {
  this: Board =>

  def nonEmptyCheck(i: Int, j: Int, c: Cell): Try[Board] = cells(i)(j) match {
    case Empty => Success(Board(cells, i, j, c))
    case _ => Failure(new Exception("Cell already occupied"))
  }

  def noSelfCaptureCheck(i: Int, j: Int, c: Cell): Try[Board] = {
    def traverse(i: Int, j: Int, visited: Set[(Int, Int)]): Boolean = {
      cells(i)(j) match {
        case Empty => true
        case x: Cell => {
          val upDowLeftRight = Array((-1, 0), (1, 0), (0, -1), (0, 1))
          val neighbors = upDowLeftRight map {case (di, dj) => (di+i, dj+j) }

          neighbors exists { case (ii, jj) =>
            !visited.contains((ii, jj)) && traverse(ii, jj, visited + ((ii, jj)))}
        }
      }
    }

    try
      val ret = traverse(i, j, Set.empty)
      if (ret) Success(Board(cells, i, j, c)) else Failure(new Exception("Is self-captured"))
    catch {
      case e: Exception => Failure(new Exception("Recursion error"))
    }
  }
  def captureOpponentCheck(): Try[Board]
  def noLoopCheck(prev: Board): Try[Board]
}

case class Board(cells: Array[Array[Cell]]) {
  this: BoardCheck =>

  def move(i: Int, j: Int, c: Cell, prev: Board): Try[Board] = {
    for (
      nonEmpty <- nonEmptyCheck(i, j, c);
      noSelfCapture <- noSelfCaptureCheck(i, j, c);
      noLoop <- noLoopCheck(prev)
    ) yield noLoop
  }
}

abstract class Cell
case object Black extends Cell
case object White extends Cell
case object Empty extends Cell

