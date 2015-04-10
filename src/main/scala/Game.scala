trait GamePlay {
  def play(start: Board, moves: Stream[Move]): Stream[Board] = {
    println("=======")
    println(start)
    moves match {
      case Stream.Empty => Stream.Empty
      case Move(i, j, c) #:: xs => start #:: play(start.move(i, j, c) getOrElse start, xs)
    }
  }
}

case class Game(rows: Int, cols: Int) extends GamePlay {
  def start() = {
    println("Game started...")

    val moves = scala.io.Source.stdin.getLines().map(line => {
      val tks = line.trim().split(" ")
      Move(tks(0).toInt, tks(1).toInt, tks(2))
    }).toStream

    play(Board(rows, cols), moves) take 100 toList
  }
}
