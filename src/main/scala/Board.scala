import scala.util.{Failure, Success, Try}


abstract class Cell extends Serializable with Product
case object Black extends Cell {
  override def toString = "b"
}
case object White extends Cell {
  override def toString = "w"
}
case object Empty extends Cell {
  override def toString = "."
}

object Move {
  def apply(i: Int, j: Int, c: String): Move = Move(i, j, if (c == "w") White else Black)
}
case class Move(i: Int, j: Int, c: Cell)


object Board {

  type Position = (Int, Int)
  val EmptyPosition = Array.empty[Position]


  def apply(cells: Array[Array[Cell]]): Board = {
    new Board(cells) with SelfCheck with NewBoardCheck with BoardTraversable
  }

  def apply(numRows: Int, numCols: Int): Board = {
    val cells = Array.ofDim[Cell](numRows, numCols)
    for (xs <- cells; i <- 0 until numCols)
      xs(i) = Empty
    Board(cells)
  }

  def apply(_cells: Array[String]): Board = {
    val cells = _cells.map(_.toCharArray map (c => c match {
      case 'w' => White
      case 'b' => Black
      case _ => Empty
    }))

    Board(cells)
  }


  def apply(_cells: Array[Array[Cell]], i: Int, j: Int, c: Cell): Board = {
    val cells = _cells.map(_.clone())
    cells(i)(j) = c
    Board(cells)
  }

  def apply(_cells: Array[Array[Cell]], captured: Array[Position]): Board = {
    val cells = _cells.map(_.clone())
    captured.foreach{ case (i, j) => cells(i)(j) = Empty }
    Board(cells)
  }
}


trait SelfCheck {
  this: BoardTraversable =>
  import Board.{Position, EmptyPosition}

  def nonEmptyCheck(board: Board, i: Int, j: Int): Try[Board] = board.cells(i)(j) match {
    case Empty => Success(board)
    case _ => Failure(new Exception("Cell already occupied"))
  }

  def captureOpponentCheck(board: Board, i: Int, j: Int, c: Cell): Try[Board] = {
    val newBoard = Board(board.cells, i, j , c)
    val captured = neighbors((i, j)) map { case (ii, jj) =>
      if (isInside(newBoard, ii, jj) && newBoard.cells(ii)(jj) != newBoard.cells(i)(j)) getCapturedCells(newBoard, ii, jj)
      else Success(EmptyPosition)
    }

    val allCaptured = captured.foldLeft(Set.empty[Position])((accum, positions) => positions match {
      case Success(xs) => accum ++ xs.toSet
      case _ => accum
    })

    Success(Board(newBoard.cells, allCaptured.toArray))
  }
}

trait BoardTraversable {
  import Board.{Position, EmptyPosition}

  def neighbors(p: Position): Array[Position] = {
    val upDownLeftRight = Array((-1, 0), (1, 0), (0, -1), (0, 1))
    upDownLeftRight map {case (di, dj) => (di+p._1, dj+p._2) }
  }

  def isInside(board: Board, i: Int, j: Int) = i >= 0 && j >= 0 && i < board.cells.length && j < board.cells(0).length
  /*
    given a board, get all captured cells starting from (si,sj) with color c
    return None if the cells starting from (si,sj) are free
   */
  def getCapturedCells(board: Board, si: Int, sj: Int): Try[Array[Position]] = {
    val color = board.cells(si)(sj)


    def isOk(i: Int, j: Int) = isInside(board, i, j) && (board.cells(i)(j) == color || board.cells(i)(j) == Empty)

    def isFree(i: Int, j: Int, visited: Set[Position]): Boolean = {
      if (board.cells(i)(j) == Empty) true
      else {
        neighbors((i, j)) exists { case (ii, jj) =>
          isOk(ii, jj) && !visited.contains((ii, jj)) && isFree(ii, jj, visited + ((ii, jj)))
        }
      }
    }

    def visit(i: Int, j: Int, visited: Set[Position]): Set[Position] = {
      (neighbors((i, j)) map {
        case (ii, jj) =>
          if (isInside(board, ii, jj) && !visited.contains((ii, jj)) && board.cells(ii)(jj) == color) visit(ii, jj, visited + ((ii, jj)))
          else Set.empty[Position]
      } reduce(_ ++ _)) + ((i, j))
    }

    try {
      if (isFree(si, sj, Set((si, sj)))) Success(EmptyPosition)
      else Success(visit(si, sj, Set((si, sj))).toArray)
    } catch {
      case e: Exception => Failure(new Exception("Recursion exception"))
    }
  }
}

trait NewBoardCheck {
  this: BoardTraversable =>
  import Board.EmptyPosition

  def noSelfCaptureCheck(board: Board, i: Int, j: Int, c: Cell): Try[Board] = {
    val newBoard = Board(board.cells, i, j, c)
    getCapturedCells(newBoard, i, j) match {
      case Failure(x: Throwable) => Failure(x)
      case Success(EmptyPosition) => Success(newBoard)
      case _ => Failure(new Exception("Is self-captured"))
    }
  }

  def noLoopCheck(prev: Board, cur: Board): Try[Board] = ???
}

class Board(val cells: Array[Array[Cell]]) {
  this: SelfCheck with NewBoardCheck =>

  def move(i: Int, j: Int, c: Cell): Try[Board] = {
    val newBoard = for (
      nonEmpty <- nonEmptyCheck(this, i, j);
      captureOpponent <- captureOpponentCheck(nonEmpty, i, j, c);
      noSelfCapture <- noSelfCaptureCheck(captureOpponent,i, j, c)
    ) yield noSelfCapture
    newBoard
  }

  override def toString = {
    cells map (row => row.mkString("")) mkString "\n"
  }
}

