trait GamePlay {
  def play(start: Board, moves: Stream[Move]): Stream[Board] = moves match {
    case Stream.Empty => Stream.Empty
    case Move(i, j, c) #:: xs => start #:: play(start.move(i, j, c) getOrElse start, xs)
  }
}

case class Game(rows: Int, cols: Int) extends GamePlay {
  def start(moves: Stream[Move]) = {
    play(Board(5, 5), moves)
  }
}
