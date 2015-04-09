
object Application {

  val game = Game(5, 5)

  val moves = scala.io.Source.stdin.getLines().map(line => {
    val tks = line.trim().split(" ")
    Move(tks(0).toInt, tks(1).toInt, tks(2))
  }).toStream

  game.start(moves)
}
