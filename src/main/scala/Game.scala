trait GamePlay {
  def play(start: Board, moves: Stream[Move]): Stream[Board] = {
    println(start)
    moves match {
      case Stream.Empty => Stream.Empty
      case Move(i, j, c) #:: xs => {
        println("=======")
        println(i+ " " + j+ " " +c)
        val next = start.move(i,j,c) getOrElse start
        next #:: play(next, xs)
      }
    }
  }
}

case class Game(rows: Int, cols: Int) extends GamePlay {
  def start() = {
    println("Game started...")

//    val moves = scala.io.Source.stdin.getLines().map(line => {
//      val tks = line.trim().split(" ")
//      Move(tks(0).toInt, tks(1).toInt, tks(2))
//    }).toStream

    val moves = Array(
      Move(0, 0, White),
      Move(0, 1, Black),
      Move(1, 1, White),
      Move(1, 0, Black),
      Move(0, 0, White)
    ).toStream

    (play(Board(rows, cols), moves) take (moves.length+1)).toList
  }
}
