import javax.activation.URLDataSource

object Day01 {
  val input = scala.io.Source.fromInputStream(
    new URLDataSource(getClass.getResource("Day01-input.txt")).getInputStream).mkString
                                                  //> input  : String = (((())))()((((((((())()(()))(()((((()(()(((()((()((()(()()
                                                  //| ()()()))(((()(()((((((((((())(()()((())()(((())))()(()(()((()(()))(()()()()(
                                                  //| (()((()(((()()(((((((()()())()((((()()(((((()(())()(())((())()()))()((((((((
                                                  //| ))(()())(()(((())(()))((())))(()((()())))()())((((())))(()(((((()(())(((()()
                                                  //| ((()((()((((((((((())(()())))))()))())()()((((()()()()()()((((((())())(((()(
                                                  //| ))()((()()(((()()()))(((((()))(((()(()()()(()(()(((())()))(()(((()((())()(()
                                                  //| ())())))((()()()(()()(((()))(((()((((()(((((()()(()())((()())())(()((((((()(
                                                  //| ()()))((((()))))())((())()()((()(()))))((((((((()))(()()(((())())(())()((()(
                                                  //| )()()((()((()((()()(((())))(()((())()((((((((()((()(()()(((())())())))(())()
                                                  //| )))()((((()))))))())))()()))()())((()())()((()()()))(()()(((()(())((((())())
                                                  //| ((((((((()()()()())))()()()((((()()))))))()((((()(((()))(()()())))((()()((((
                                                  //| )))()()())())(((())((()()(())()()()(((())))))()())((()))()))((())()()())()()
                                                  //| )()()(()))())))())()))((
                                                  //| Output exceeds cutoff limit.

  val Up = '('                                    //> Up  : Char = (
  val Down = ')'                                  //> Down  : Char = )

  def whatFloor: String => Int = puzzle => puzzle.count(_ == Up) + puzzle.count(_ == Down) * (-1)
                                                  //> whatFloor: => String => Int

  assert(whatFloor("(())") == 0)
  assert(whatFloor("()()") == 0)
  assert(whatFloor("(((") == 3)
  assert(whatFloor("(()(()(") == 3)
  assert(whatFloor("))(((((") == 3)
  assert(whatFloor("())") == -1)
  assert(whatFloor("))(") == -1)
  assert(whatFloor(")))") == -3)
  assert(whatFloor(")())())") == -3)

  val solution = whatFloor(input)                 //> solution  : Int = 232

  def findFloor(puzzle: String, currentFloor: Int = 0, floorToFind: Int, position: Int = 0): Option[Int] = {
    if (currentFloor == floorToFind)
      Some(position)
    else if (puzzle.isEmpty)
      None
    else
      findFloor(
        puzzle.tail,
        currentFloor + (if (puzzle.head == Up) 1 else -1),
        floorToFind,
        position + 1)
  }                                               //> findFloor: (puzzle: String, currentFloor: Int, floorToFind: Int, position: 
                                                  //| Int)Option[Int]
  assert(findFloor(puzzle = ")", floorToFind = -1) == Some(1))
  assert(findFloor(puzzle = "()())", floorToFind = -1) == Some(5))
  findFloor(puzzle = input, floorToFind = -1)     //> res0: Option[Int] = Some(1783)
}