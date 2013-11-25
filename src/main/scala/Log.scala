import java.io.{File, FileWriter, BufferedWriter}
import scala.io.Source

import scala.collection.mutable.HashMap

/**
 * Transaction log
 */
class Log(fname: String) {
  val entries = new HashMap[String, String]
  private val hasOldLog = new File(fname).exists()

  if(hasOldLog) {
    val src = Source.fromFile(fname)
    src.getLines().foreach { l =>
      val Array(k, v) = l.split(' ')
      entries += k -> v
    }
    src.close()
  }

  private val out = new BufferedWriter(new FileWriter(fname, hasOldLog))

  def append(k: String, v: String) {
    val k1 = k.replace(' ', '_')
    val v1 = v.replace(' ', '_')
    entries += k1 -> v1
    out.write(s"$k1 $v1\n")
    out.flush()
  }

  def close() = out.close()

  def getGithubIssueFor(id: String): Option[Long] = entries.get(s"ticket/$id").map(_.toLong)
  def setGithubIssueFor(id: String, ghNum: Long) = append(s"ticket/$id", ghNum.toString)

  def isIssueClosed(id: String) = entries.contains(s"issue-closed/$id")
  def setIssueClosed(id: String) = append(s"issue-closed/$id", ".")

  def isCommentDone(id: String) = entries.contains(s"comment/$id")
  def setCommentDone(id: String) = append(s"comment/$id", ".")

  def isAssociationDone(id: String) = entries.contains(s"assoc/$id")
  def setAssociationDone(id: String) = append(s"assoc/$id", ".")
}
