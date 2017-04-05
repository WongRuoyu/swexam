package com.wong
package snippet

import net.liftweb.http.{S, StatefulSnippet}
import net.liftweb.util.Helpers._
import net.liftweb.mapper.By
import net.liftweb.mapper.view.MapperPaginatorSnippet
import net.liftweb.common.Full
import com.wong.model.{QuestionKnowledgepts, Question}

/**
 * Created by Wong on 15-8-2.
 */
class QuestionPaginatorSnippet extends StatefulSnippet with QuestionSptHelper {
  override def dispatch: DispatchIt = {
    case "pages" => pages
    case "paginate" => paginator.paginate _
  }

  def pages = "#question_pages *" #> this.paginator.page.map {
    question => single(question) &
      "#detail [href]" #> "/question/show/%s".format(question.id.get)
  }

  private val paginator = new MapperPaginatorSnippet[Question](Question) {
    override def itemsPerPage = 5

    override def count = questions.size

    val questions: List[Question] = S.param("kp_id") match {
      case Full(id) if id.toLong != 0 => QuestionKnowledgepts.findAll(By(QuestionKnowledgepts.knowledgept, id.toLong)).map(_.question.obj.map(q => q)).flatten
      case _ => Question.findAll()
    }

    override def page = questions.slice(curPage * itemsPerPage, (curPage + 1) * itemsPerPage)
  }

}

