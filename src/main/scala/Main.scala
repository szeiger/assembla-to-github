import org.kohsuke.github.{GitHub, GHIssue}
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.JavaConversions._

object Main extends App {
  // Configure the app here:
  val repoName = "slick/issues-test"
  val dumpFile = "dump.js"
  val logFile = "log.txt"
  val create = true
  val labelAll = "migrated"
  // Map Assembla users to Github users
  val userMapping = new HashMap[String, String] ++ Seq(
    "d4p4jekUOr4jZdeJe5cbLA" -> "cvogt",
    "a6trRkA24r4l5xeJe5cbCb" -> "szeiger",
    "bl9rLQEs4r3AU0ab7jnrAJ" -> "xeno-by",
    "dY-d9Y4q8r3Qa0eJe5afGb" -> "bwmcadams",
    "byWDaK_W8r4l_XacwqjQXA" -> "amirsh",
    "do25aqeq4r34vueJe5afGb" -> "nafg",
    "dHsulKvxyr4iggeJe5cbLr" -> "dotta",
    "cw1KuKVAKr3OgceJe5afGb" -> "jboner"
  )
  // Map Assembla milestones to Github milestones and labels
  val milestoneMapping = new HashMap[String, (String, String)] ++ Seq(
    "Closed Iteration 2013-07-10" -> ("", ""),
    "Closed Iteration 2013-04-17" -> ("", ""),
    "Closed Iteration 2013-05-22" -> ("", ""),
    "Closed Iteration 2013-05-01" -> ("", ""),
    "Closed Iteration 2013-08-26" -> ("", ""),
    "Closed Iteration 2013-09-04" -> ("", ""),
    "0.10-M2"   -> ("0.10-M2", ""),
    "0.11"      -> ("0.11",    ""),
    "1.0"       -> ("1.0",     ""),
    "Backlog"   -> ("Future",  ""),
    "Slick 2.1" -> ("2.1",     ""),
    "Current"   -> ("2.0",     "1 - Ready"),
    "Slick 2.0" -> ("2.0",     "")
  )
  // Map Assembla statuses to Github statuses and closed flag
  val ticketStatusMapping = new HashMap[String, (String, Boolean)] ++ Seq(
    "New"       -> ("", false),
    "Accepted"  -> ("2 - Working", false),
    "Test"      -> ("review-pending", false),
    "Invalid"   -> ("invalid", true),
    "Fixed"     -> ("", true),
    "Won't Fix" -> ("wontfix", true)
  )
  def ticketFilter(tickets: Vector[Ticket]): Vector[Ticket] =
    tickets
  def ticketHeader(user: String, url: String, num: Long, date: String) =
    s"*[Migrated from Assembla ticket [$num]($url) - reported by @$user on $date]*\n\n"
  def commentHeader(user: String, url: String, num: Long, date: String) =
    s"*[Migrated from comment on Assembla ticket [$num]($url) - comment by @$user on $date]*\n\n"
  def associationComment(num: Long, kind: String) =
    s"*[Migrated from Assembla: Related issue: #$num ($kind)]*"
  def existingGithubIssueFor(t: Ticket): Option[Long] = if(t.summary.startsWith("GH#")) {
    Some(t.summary.substring(3).split(": ".toCharArray).apply(0).toLong)
  } else None


  // Read tickets and filter for the ones we care about
  println(s"Parsing Assembla dump in $dumpFile...")
  val ass = new Assembla(dumpFile)
  println("Read " + ass.tickets.size + " tickets.")
  val tickets = ticketFilter(ass.tickets.values.toVector.sortBy(_.number))

  // Check for missing users
  val missingUsers =
    tickets.flatMap { t => t.reporterId +: t.assignedToId +: t.comments.map(_.userId) }.toSet.filterNot { u =>
      u == null || userMapping.contains(u)
    }
  if(missingUsers.nonEmpty) {
    println("Missing user mappings: "+missingUsers.map(u => "\""+u+"\" -> \"\"").mkString(", "))
    System.exit(1)
  }

  // Check for missing milestones
  val usedMilestones = tickets.flatMap(t => Option(t.milestone).map(_.title)).toSet
  val missingMilestones = usedMilestones.filterNot { m => m == null || milestoneMapping.contains(m) }
  if(missingMilestones.nonEmpty) {
    println("Missing milestone mappings: "+missingMilestones.map(m => "\""+m+"\" -> (\"\", \"\")").mkString(", "))
    System.exit(1)
  } else println("Using milestones: " + usedMilestones.mkString(", "))

  // Check for missing ticket statuses
  val usedStatuses = tickets.map(_.ticketStatus.name).toSet
  val missingStatuses = usedStatuses.filterNot { s => ticketStatusMapping.contains(s) }
  if(missingStatuses.nonEmpty) {
    println("Missing ticket status mappings: "+missingStatuses.map(m => "\""+m+"\" -> (\"\", false)").mkString(", "))
    System.exit(1)
  } else println("Using ticket statuses: " + usedStatuses.mkString(", "))

  // Connect to github and find the github users
  println("Finding users on github...")
  val gh = GitHub.connect()
  val ghUsers = userMapping.map { case (k, v) => (k, gh.getUser(v)) }
  println("  All users found.")

  // Find the github milestones
  println("Finding milestones on github...")
  val repo = gh.getRepository(repoName)
  val ghAllMilestones = repo.getMilestones.values().map(m => m.getTitle -> m).toMap
  val ghMilestones = milestoneMapping.map { case (k, m) =>
    // should check labels, too, but the github Java client doesn't support that
    (k, ( if(m._1 == "") null else ghAllMilestones(m._1),
      m._2))
  }
  println("  All milestones found.")

  // Open transaction log
  println("Opening transaction log "+logFile+"...")
  val log = new Log(logFile)
  try {

    // Create tickets on github
    val ticketIdToIssue = new HashMap[String, GHIssue]
    if(create) println("Creating "+tickets.size+" tickets...")
    else println("Walking through "+tickets.size+" tickets (create=false)...")
    tickets.foreach { t =>
      println("At Assembla ticket "+t.number)
      val loggedIssue = log.getGithubIssueFor(t.id)
      val issue = loggedIssue match {
        case Some(num) =>
          println("Found logged issue #"+num)
          Some(repo.getIssue(num.toInt))
        case None =>
          existingGithubIssueFor(t) match {
            case Some(id) =>
              println("  Matched with existing github issue #"+id)
              Some(repo.getIssue(id.toInt))
            case None =>
              val ib = repo.createIssue(t.summary)
              var desc = ticketHeader(userMapping(t.reporterId), "https://www.assembla.com/spaces/"+ass.space.wikiName+"/tickets/"+t.number, t.number, formatDate(t.createdOn)) +
                Option(t.description).getOrElse("")
              if(desc != "") ib.body(desc)
              if(t.assignedToId != null) ib.assignee(userMapping(t.assignedToId))
              if(t.milestone != null) {
                println("  Translating milestone "+t.milestone.title)
                val ghmOpt = ghMilestones.get(t.milestone.title)
                ghmOpt.foreach { case (m, l) =>
                  if(m != null) ib.milestone(m)
                  if(l != "") ib.label(l)
                }
              }
              val (statusLabel, close) = ticketStatusMapping(t.ticketStatus.name)
              if(statusLabel != "") ib.label(statusLabel)
              if(labelAll != "") ib.label(labelAll)
              if(create) {
                println("  Creating on github...")
                val issue = ib.create()
                println("  Created github issue #"+issue.getNumber)
                log.setGithubIssueFor(t.id, issue.getNumber)
                if(close) {
                  if(log.isIssueClosed(t.id))
                    println("  Issue closure is already logged.")
                  else {
                    println("  Closing issue...")
                    issue.close()
                    log.setIssueClosed(t.id)
                    println("  Closed issue #"+issue.getNumber)
                  }
                }
                Some(issue)
              } else None
          }
      }
      issue.foreach(i => ticketIdToIssue += t.id -> i)
      t.comments.foreach { c =>
        println("    At comment "+c.id)
        val desc = commentHeader(userMapping(c.userId), "https://www.assembla.com/spaces/"+ass.space.wikiName+"/tickets/"+t.number+"#comment:"+c.id, t.number, formatDate(c.createdOn)) +
          Option(c.comment).getOrElse("")
        if(create) {
          if(log.isCommentDone(c.id))
            println("    Comment is already logged.")
          else {
            println("    Creating on github...")
            if(desc != "") issue.get.comment(desc)
            log.setCommentDone(c.id)
            println("    Created comment.")
          }
        }
      }
    }

    if(create) {
      println("Creating associations...")
      tickets.foreach { t =>
        println("At Assembla ticket "+t.number)
        val issue = ticketIdToIssue(t.id)
        t.assocFrom.foreach { a =>
          println("    At association to "+a.ticket2Id+" ("+a.relName+")")
          if(create) {
            if(log.isAssociationDone(a.id))
              println("    Association is already logged.")
            else {
              val target = ticketIdToIssue(a.ticket2Id)
              println("    Creating on github...")
              issue.comment(associationComment(target.getNumber, a.relName))
              log.setAssociationDone(a.id)
              println("    Created comment.")
            }
          }
        }
      }
    }

  } finally {
    println("Closing transaction log...")
    log.close()
  }

  println("Migration successfully completed.")

  def formatDate(s: String): String =
    (if(s.endsWith("+00:00")) s.substring(0, s.length-6) else s).replace('T', ' ')
}
