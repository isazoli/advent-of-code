object Day04PartTwo {
  val md5Digest = java.security.MessageDigest.getInstance("MD5")

  def md5: String => Int => Array[Byte] = base => num => md5Digest.digest((base + num).getBytes)
  val md5FunctionWithBgvyzdsv = md5("bgvyzdsv")
  assert(Some(1038736) == Stream.from(1).find(
    md5FunctionWithBgvyzdsv(_).slice(0, 3) match {
      case Array(0, 0, 0) => true
      case _ => false
    }))
}