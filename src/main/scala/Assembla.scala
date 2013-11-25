import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.io.Source
import scala.util.parsing.json.JSON

/**
 * Contains the Assembla space backup data.
 */
class Assembla(fileName: String) {
  val milestones = new HashMap[String, Milestone]
  val ticketStatuses = new HashMap[String, TicketStatus]
  val tickets = new HashMap[String, Ticket]
  var space: Space = _

  JSON.perThreadNumberParser = { (s: String) => s }
  for(l <- Source.fromFile(fileName).getLines()) {
    val sep = l.indexOf(",")
    val tpe = l.substring(0, sep).trim
    val data = l.substring(sep+1).trim
    parse(tpe, JSON.parseFull(data).get.asInstanceOf[Map[String, Any]])
  }

  def parse(tpe: String, data: => Map[String, Any]) {
    def s(key: String): String = data(key).asInstanceOf[String]
    tpe match {
      case "spaces" =>
        space = Space(s("wiki_name"))
      case "milestones" =>
        milestones += s("id") -> Milestone(s("id"), s("title"))
      case "ticket_statuses" =>
        ticketStatuses += s("id") -> TicketStatus(s("id"), s("name"))
      case "tickets" =>
        tickets += s("id") -> Ticket(
          s("id"), s("number").toLong, s("summary"))(s("reporter_id"), s("assigned_to_id"),
          s("description"), s("created_on"), milestones.get(s("milestone_id")).getOrElse(null),
          ticketStatuses(s("ticket_status_id"))
        )
      case "ticket_comments" =>
        val comment = s("comment")
        if(comment != null && comment != "") {
          val tc = TicketComment(s("id"), s("ticket_id"), s("user_id"), s("created_on"), comment)
          tickets(tc.ticketId).comments += tc
        }
      case "ticket_associations" =>
        val assoc = TicketAssociation(s("id"), s("ticket1_id"), s("ticket2_id"), s("relationship").toInt)
        tickets(assoc.ticket1Id).assocFrom += assoc
        tickets(assoc.ticket2Id).assocTo += assoc
      case _ =>
    }
  }
}

case class Milestone(id: String, title: String)
case class Ticket(id: String, number: Long, summary: String)(val reporterId: String, val assignedToId: String,
                                                             val description: String, val createdOn: String,
                                                             val milestone: Milestone, val ticketStatus: TicketStatus) {
  val comments = new ArrayBuffer[TicketComment]()
  val assocFrom = new ArrayBuffer[TicketAssociation]()
  val assocTo = new ArrayBuffer[TicketAssociation]()
}
case class TicketStatus(id: String, name: String)
case class TicketComment(id: String, ticketId: String, userId: String, createdOn: String, comment: String)
case class TicketAssociation(id: String, ticket1Id: String, ticket2Id: String, relationship: Int) {
  def relName = relationship match {
    case 0 => "Parent"
    case 1 => "Child"
    case 2 => "Related"
    case 3 => "Duplicate"
    case 4 => "Sibling"
    case 5 => "Story"
    case 6 => "Subtask"
    case 7 => "Dependent"
    case 8 => "Block"
    case _ => "Unknown"
  }
}
case class Space(wikiName: String)
