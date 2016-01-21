object Day03 {

  val map = "^v^v^v^v^v"
  //> map  : String = ^v^v^v^v^v
  def splitInput: String => (String, String) = s => {
    val (listOdd, listEven) = s.zipWithIndex
      .partition(_._2 % 2 == 0)
    (listOdd.map(_._1).mkString, listEven.map(_._1).mkString)
  }

  //> splitInput: => String => (String, String)

  case class Position(x: Int, y: Int)
  val move = Map[Char, (Position => Position)](
    '<' -> { p => Position(p.x - 1, p.y) },
    '>' -> { p => Position(p.x + 1, p.y) },
    '^' -> { p => Position(p.x, p.y - 1) },
    'v' -> { p => Position(p.x, p.y + 1) }) //> move  : scala.collection.immutable.Map[Char,day3.Position => day3.Position]
  //| = Map(< -> <function1>, > -> <function1>, ^ -> <function1>, v -> <function1>
  //| )

  def calc: String => Set[Position] = _.foldLeft(Seq(Position(0, 0)))((sum, current) => sum.+:(move(current)(sum.head)))
    .toSet //> calc: => String => Set[day3.Position]
  val input = scala.io.Source.fromInputStream(getClass.getResourceAsStream("Day03-input.txt"))
  //> input  : scala.io.BufferedSource = non-empty iterator

  assert(2 == calc(">").size)
  assert(4 == calc("^>v<").size)
  assert(2 == calc("^v^v^v^v^v").size)
  assert(2592 == calc(input.getLines().mkString).size)
  def calcRoboSanta: String => Set[Position] = s => {
    val (o, e) = splitInput(s)
    calc(o).++:(calc(e))
  } //> calcRoboSanta: => String => Set[day3.Position]
  assert(3 == calcRoboSanta("^v").size)
  assert(3 == calcRoboSanta("^>v<").size)
  assert(11 == calcRoboSanta("^v^v^v^v^v").size)
  val input2 = scala.io.Source.fromInputStream(getClass.getResourceAsStream("Day03-input.txt"))
  //> input2  : scala.io.BufferedSource = non-empty iterator
  assert(2360 == calcRoboSanta(input2.getLines().mkString).size) //> res0: Int = 2360
}