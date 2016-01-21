import java.io.InputStream

object Day06 {

  type Command = Int => Int

  def turnOnCmd: Command = _ => 1

  def turnOffCmd: Command = _ => 0

  def toggleCmd: Command = ~_

  case class Coordinate(x: Int, y: Int, state: Int = 0) {
    def set(c: Command): Coordinate = Coordinate(x, y, c(state))
  }

  val first10Lines: Iterator[String] = List(
    "toggle 461,550 through 564,900",
    "turn off 370,39 through 425,839",
    "turn off 464,858 through 833,915",
    "turn off 812,389 through 865,874",
    "turn on 599,989 through 806,993",
    "turn on 376,415 through 768,548",
    "turn on 606,361 through 892,600",
    "turn off 448,208 through 645,684",
    "toggle 50,472 through 452,788",
    "toggle 205,417 through 703,826"
  ).toIterator
  val CommandWithGridRange = "(\\D+)(\\d+),(\\d+)through(\\d+),(\\d+)".r
  "toggle461,550through564,900" match {
    case CommandWithGridRange(c, d1, d2, d3, d4) => println(c + "@" + d2 + "#" + d3 + "@" + d4)
  }
  val commandsByCode: Map[String, Command] = Map("toggle" -> toggleCmd, "turnon" -> turnOnCmd, "turnoff" -> turnOffCmd)

  def generateCoordinates: Command => Coordinate => Coordinate => Seq[(Coordinate, Command)] =
    command => startCoordinate => endCoordinate =>
      for {
        x <- startCoordinate.x to endCoordinate.x
        y <- startCoordinate.y to endCoordinate.y
      } yield (Coordinate(x, y), command)

  def translateToCommand: Map[String, Command] => String => (Command, Coordinate, Coordinate) =
    commandsByCode => s => s.replaceAll("\\s", "") match {
      case CommandWithGridRange(c, d1, d2, d3, d4) => (
        commandsByCode(c),
        Coordinate(d1.toInt, d2.toInt),
        Coordinate(d3.toInt, d4.toInt)
        )
    }

  def transformGrid: InputStream => Map[String, Command] => Array[Array[Coordinate]] = is => commandsTranslator => {
    val gridSize = 1000
    val grid: Array[Array[Coordinate]] = Array.tabulate[Coordinate](gridSize, gridSize) { (i, j) => new Coordinate(i, j) }
    val lines = scala.io.Source.fromInputStream(is).getLines()
    lines
      .map(translateToCommand(commandsTranslator))
      .flatMap { case (cmd, coord1, coord2) => generateCoordinates(cmd)(coord1)(coord2) }
      .foreach {
      case (coordinate, command) => grid(coordinate.x)(coordinate.y) = grid(coordinate.x)(coordinate.y).set(command)
    }
    grid
  }

  assert(359036 == transformGrid(getClass.getResourceAsStream("Day06-input1.txt"))(commandsByCode).flatten.count(_.state == 1))
  /** Part 2 **/
  def turnOnCmd2: Command = _ + 1

  def turnOffCmd2: Command = x => Math.max(0, x - 1)

  def toggleCmd2: Command = _ + 2

  val commandsByCode2 = Map("toggle" -> toggleCmd2, "turnon" -> turnOnCmd2, "turnoff" -> turnOffCmd2)

  assert(14687245 == transformGrid(getClass.getResourceAsStream("Day06-input1.txt"))(commandsByCode2)
    .flatten.aggregate(0)(_ + _.state, _ + _))
}
