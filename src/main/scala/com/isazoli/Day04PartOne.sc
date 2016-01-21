object Day04PartOne {

  import java.security.MessageDigest

  val md5Digest = MessageDigest.getInstance("MD5")

  def md5: String => Int => Array[Byte] =
    base => number => md5Digest.digest((base + number.toString).getBytes)

  def bytes2hex(bytes: Array[Byte]): Array[String] = bytes.map("%02x".format(_))

  bytes2hex(md5("abcdef")(609043))
  val x = Stream
    .from(1)
    .map(idx => (idx, bytes2hex(md5("bgvyzdsv")(idx)).toList))
    .collectFirst {
    case (idx, "00" :: "00" :: "00" :: tail) => idx
  }
}