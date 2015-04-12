trait GamePlay {
  def play(prev: Board, current: Board, moves: Stream[Move]): Stream[Board] = {
    println(current)
    moves match {
      case Stream.Empty => Stream.Empty
      case Move(i, j, c) #:: xs => {
        println("=======")
        println(i+ " " + j+ " " +c)
        val next = current.move(prev, i,j,c) getOrElse current
        next #:: play(current, next, xs)
      }
    }
  }
}

case class Game(rows: Int, cols: Int) extends GamePlay {
  def start() = {
    println("Game started...")

    val moves = Array(
      Move(0, 0, White),
      Move(0, 1, Black),
      Move(1, 1, White),
      Move(1, 0, Black),
      Move(0, 0, White)
    ).toStream

    val emptyBoard = Board(rows, cols)
    (play(emptyBoard, emptyBoard, moves) take (moves.length+1)).toList
  }
}
