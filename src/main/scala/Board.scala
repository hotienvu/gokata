import scala.util.{Failure, Success, Try}

abstract class Cell
case object Black extends Cell
case object White extends Cell
case object Empty extends Cell

object Board {

  type Position = (Int, Int)

  def apply(numRows: Int, numCols: Int): Board = {
    val cells = Array.ofDim[Cell](numRows, numCols)
    for (xs <- cells; i <- 0 until numCols)
      xs(i) = Empty
    new Board(cells) with SelfCheck with NewBoardCheck with BoardTraversable
  }

  def apply(_cells: Array[Array[Cell]], i: Int, j: Int, c: Cell): Board = {
    val cells = _cells.clone()
    cells(i)(j) = c
    new Board(cells) with SelfCheck with NewBoardCheck with BoardTraversable
  }

  def apply(_cells: Array[Array[Cell]], captured: Array[Position]): Board = ???
}


trait SelfCheck {
  this: Board with BoardTraversable =>

  def nonEmptyCheck(i: Int, j: Int, c: Cell): Try[Board] = cells(i)(j) match {
    case Empty => Success(this)
    case _ => Failure(new Exception("Cell already occupied"))
  }


  def captureOpponentCheck(i: Int, j: Int, c: Cell): Try[Board] = ???
}

trait BoardTraversable {
  import Board.Position

  def neighbors(p: Position): Array[Position] = {
    val upDowLeftRight = Array((-1, 0), (1, 0), (0, -1), (0, 1))
    upDowLeftRight map {case (di, dj) => (di+p._1, dj+p._2) }
  }
  /*
    given a board, get all captured cells starting from (si,sj) with color c
    return None if the cells starting from (si,sj) are free
   */
  def traverse(board: Board, si: Int, sj: Int): Try[Array[Position]] = {
    val color = board.cells(si)(sj)

    def isFree(i: Int, j: Int, visited: Set[Position]): Boolean = {
      if (board.cells(i)(j) == Empty) true
      else {
        neighbors((i, j)) exists { case (ii, jj) => !visited.contains((ii, jj)) && board.cells(ii)(jj) == color && isFree(ii, jj, visited + ((ii, jj)))}
      }
    }

    def visit(i: Int, j: Int, visited: Set[Position]): Set[Position] = {
      if (visited.contains((i, j))) visited
      else {
        val upDowLeftRight = Array((-1, 0), (1, 0), (0, -1), (0, 1))
        val neighbors = upDowLeftRight map {case (di, dj) => (di+i, dj+j) }
        neighbors map {
          case (ii, jj) => if (!visited.contains((ii, jj)) && board.cells(ii)(jj) == color) visit(ii, jj, visited + ((ii, jj))) else Set.empty[Position]
        } reduce(_ ++ _)
      }
    }

    try {
      if (isFree(si, sj, Set((si, sj)))) Success(Array.empty)
      else Success(visit(si, sj, Set((si, sj))).toArray)
    } catch {
      case e: Exception => Failure(new Exception("Recursion exception"))
    }
  }
}

trait NewBoardCheck {
  this: BoardTraversable =>

  def noSelfCaptureCheck(board: Board, i: Int, j: Int, c: Cell): Try[Board] = {
      val newBoard = Board(board.cells, i, j, c)
      traverse(newBoard, i, j) match {
        case Failure(x: Throwable) => Failure(x)
        case Success(Array.empty) => Success(newBoard)
        case _ => Failure(new Exception("Is self-captured"))
      }
  }


  def noLoopCheck(prev: Board, cur: Board): Try[Board] = ???
}

class Board(val cells: Array[Array[Cell]]) {
  this: SelfCheck with NewBoardCheck =>

  def move(i: Int, j: Int, c: Cell, prev: Board): Try[Board] = {
    for (
      nonEmpty <- nonEmptyCheck(i, j, c);
      captureOpponent <- captureOpponentCheck(i, j, c);
      noSelfCapture <- noSelfCaptureCheck(captureOpponent,i, j, c);
      noLoop <- noLoopCheck(prev, noSelfCapture)
    ) yield noLoop
  }
}

