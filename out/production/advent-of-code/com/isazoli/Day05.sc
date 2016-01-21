object Day05 {
  type Rule = String => Boolean

  val rulesForPart01: List[Rule] = List(
    s => Set("ab", "cd", "pq", "xy").forall(!s.contains(_)),
    _.filter(Set('a', 'e', 'i', 'o', 'u').contains).length >= 3,
    doubleOccurrenceRule
  )

  def doubleOccurrenceRule: String => Boolean = input => input.zip(input.tail).exists { case (a, b) => a == b }

  def isNice: List[Rule] => String => Boolean = rules => str => rules.forall(_(str))

  val isNicePart1 = isNice(rulesForPart01)

  assert(isNicePart1("ugknbfddgicrmopn"))
  assert(isNicePart1("aaa"))
  assert(!isNicePart1("jchzalrnumimnmhp"))
  assert(!isNicePart1("haegwjzuvuyypxyu"))
  assert(!isNicePart1("dvszwmarrgswjxmb"))

  val inputFile = scala.io.Source.fromInputStream(getClass.getResourceAsStream("Day05-input1.txt"))

  assert(238 == inputFile.getLines().count(isNicePart1))

  def shiftLeftByTwo: String => String = _.tail.tail

  /** Part 2 **/
  def doubleDoubleCheck: String => Boolean = input => {
    val (indexesByPairs, _) = input.zipWithIndex.foldLeft((Map[String, List[Int]](), 0: Char)) {
      case ((sum, lastChar), (currentChar, currentIdx)) =>
        val currentKey = String.valueOf(lastChar, currentChar)
        (sum.+(currentKey -> sum.getOrElse(currentKey, List.empty).:+(currentIdx)), currentChar)
    }
    val (_, indexes) = indexesByPairs.unzip
    indexes.exists(idxList =>
      idxList.length > 2 ||
        idxList.zip(idxList.tail).exists { case (i1, i2) => (i2 - i1) > 1 }
    )
  }
  assert(doubleDoubleCheck("xyxy"))
  assert(doubleDoubleCheck("aabcdefgaa"))
  assert(!doubleDoubleCheck("aaa"))

  def doubleWithOneInTheMiddle: String => Boolean = input => input.zip(shiftLeftByTwo(input)).exists { case (a, b) => a == b }
  assert(doubleWithOneInTheMiddle("xyx"))
  assert(doubleWithOneInTheMiddle("abcdefeghi"))
  assert(doubleWithOneInTheMiddle("aaa"))

  val rulesForPart02: List[Rule] = List(doubleDoubleCheck, doubleWithOneInTheMiddle)
  val isNicePart2 = isNice(rulesForPart02)
  assert(isNicePart2("qjhvhtzxzqqjkmpb"))
  assert(isNicePart2("xxyxx"))
  assert(!isNicePart2("uurcxstgmygtbstg"))
  assert(!isNicePart2("ieodomkazucvgmuy"))

  assert(69 == scala.io.Source.fromInputStream(getClass.getResourceAsStream("Day05-input1.txt")).getLines().count(isNicePart2))
}