package com.wong
package snippet

import net.liftweb.http.{RequestVar, S, StatefulSnippet}
import net.liftweb.http.SHtml._

import com.wong.model.{QuestionKnowledgepts, KnowledgePoint, Question}
import net.liftweb.util.Helpers._
import net.liftweb.mapper.view.MapperPaginatorSnippet
import net.liftweb.mapper.{By, MaxRows, StartAt}
import scala.xml.{NodeSeq, Text}
import net.liftweb.common.{Box, Full, Empty}

/**
 * Created by Wong on 15-8-5.
 */

class QuestionSnippet extends StatefulSnippet with QuestionSptHelper {
  override def dispatch: DispatchIt = {
    case "allQuestions" => all
    case "detail" => detail
    case "paginate" => paginator.paginate _
    case "opts" => opts
    case "confirmDelete" => confirmDelete
    case "edit" => edit
    case "pages" => pages
  }

  private object selectedQest extends RequestVar[Box[Question]](Empty)

  def pages = "#question_pages *" #> this.paginator.page.map {
    question => single(question) &
      "#detail [href]" #> "/question/show/%s".format(question.id.get)
  }

  private val paginator = new MapperPaginatorSnippet[Question](Question) {
    override def itemsPerPage = 5

    override def count = Question.findAll().size

    lazy val offset = S.param("offset").map(_.toInt).openOr(0)

    override def page = Question.findAll(StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))
  }


  def opts = {
    val kpts = KnowledgePoint.findAll()
    "#kpts *" #> kpts.map {
      kpt =>
        a(() => filt(kpt.name.toString), Text(kpt.name.toString))
    }
  }

  def confirmDelete = {
    (for (question <- selectedQest.is) yield {
      def deleteQ() {
        println("Can delete?: " + question.db_can_delete_?)
        S.notice("delete", "Question " + question.id.get + "deleted.")
        question.delete_!
        println("delete", "Question " + question.id.get + "deleted.")
        S.redirectTo("/question/questions.html")
      }
      ".content" #> question.content.get &
        ".delete" #> onSubmitUnit(deleteQ _)
    }) openOr {
      println("error in deleteing question")
      S.error("delete", "selected question not exist.");
      redirectTo("/question/questions.html")
    }
  }

  private def saveQuestion(question: Question) = {
    println("question's content: " + question.content.get.toString)
    question.validate match {
      case Nil => question.save; println("edited question saved.."); redirectTo("/question/questions.html")
      case error => S.error("edit_error", error.mkString("")); println("error in editing question.." + error.mkString("")); selectedQest(Full(question))
    }
  }

  def edit(in: NodeSeq): NodeSeq =
    selectedQest.map(_.toForm(Full("save"), saveQuestion _) ++ <div>
      <a href="/question/questions.html">Back</a>
    </div>) openOr {
      S.error("question not found.");
      redirectTo("/question/questions.html")
    }

  private def filt(kpt: String) = {
    val kp = KnowledgePoint.find(By(KnowledgePoint.name, kpt)).head
    redirectTo("/question/filter/" + kp.id.get)
  }

  lazy val q = Question.find(By(Question.id, S.param("id").map(_.toLong).openOr(0L)))

  def detail = q.map {
    question =>
      single(question) &
        ".kp *" #> question.knowledgepts.map(_.name.toString).reduce(_ + _)
  } openOr ("*" #> "That question does not exist!")

  def all = {
    val questions: List[Question] = S.param("kp_id") match {
      case Full(id) => QuestionKnowledgepts.findAll(By(QuestionKnowledgepts.knowledgept, id.toLong)).map(_.question.obj.map(q => q)).flatten
      case _ => Question.findAll()
    }
    "#questions *" #> questions.map {
      question => single(question) &
        "#detail [href]" #> "/question/show/%s".format(question.id.get) &
        "#modify *" #> link("/question/edit", () => selectedQest(Full(question)), Text("Edit")) &
        "#delete *" #> link("/question/delete", () => selectedQest(Full(question)), Text("Delete"))
    }
  }

}

trait QuestionSptHelper {
  def single(question: Question) = {
    ".id *" #> question.id &
      ".content *" #> question.content &
      ".point *" #> question.points
  }
}
