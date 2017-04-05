package com.wong
package snippet

import net.liftweb.http.{S, RequestVar, StatefulSnippet, SHtml}
import com.wong.model._
import net.liftweb.util.Helpers._
import net.liftweb.common.{Full, Box, Empty}
import scala.xml.{XML, Text}
import net.liftweb.mapper.By
import net.liftweb.common.Full

/**
 * Created by Wong on 15-8-11.
 */
class PublicExamSnippet extends StatefulSnippet {
  override def dispatch: DispatchIt = {
    case "enterExam" => enterExam
    case "doexam" => doexam
    case "examPost" => examPost
  }

  def examPost = {
    val examHist_id = S.param("examHist_id").map(_.toLong).openOr(0L)
    val examHist = ExamHistory.find(By(ExamHistory.id, examHist_id))
    val examId = examHist.map(_.exam.get).openOr(0L)
    val points = examHist.map(_.total_points.get.toString).openOr("0")
    val exam_date = examHist.map(_.createdAt.get.toString).openOr("not known")
    val exam = Exam.find(By(Exam.id, examId))
    val name = exam.map(_.name.get).openOr("no name...")
    "#exam-name *" #> name &
      "#exam-date *" #> exam_date &
      "#exam-points *" #> points
  }


  def doexam = (for (exam <- selectedExam.is) yield {
    def onSubmit = {
      val pairs: List[(Long, String)] = for {req <- S.request.toList
                                             paramName <- req.paramNames if paramName.matches("[0-9]*")
      } yield {
        paramName.toLong -> S.params(paramName).mkString("-")
      }

      var points_get = 0
      for ((id, answer) <- pairs) {
        val question = Question.find(By(Question.id, id)).openOrThrowException("Question from request no found!")
        val wr = question.wrongTimes.get
        val ans = question.answer.get
        if (answer.trim.equalsIgnoreCase(ans)) {
          //此题作对
          points_get += question.points.get
          User.currentUser.map(UserQuestions.join(_, question, true))
        } else {
          //此题做错
          question.wrongTimes(wr + 1).save()
          //保存用户做错此题
          User.currentUser.map(UserQuestions.join(_, question, false))
        }
      }
      User.currentUser.map(_.finishExam(exam))
      //用户提交试卷记录
      val examHis = User.currentUser.map(ExamHistory.join(_, exam, points_get, pairs.mkString("&"))).openOr(ExamHistory.create)
      S.redirectTo("/public/result/%s".format(examHis.id.get))
    }
    ".exam-title *" #> exam.name.get &
      ".exam-descp" #> exam.description.get &
      "#questions" #> XML.loadString("<div class='questions'>" + exam.content.get + "</div>") &
      "#submit" #> SHtml.submit("Submit", () => onSubmit)
  }) openOr {
    S.redirectTo("/index.html")
  }

  object selectedExam extends RequestVar[Box[Exam]](Empty)

  object selectedExamHist extends RequestVar[Box[ExamHistory]](Empty)

  def enterExam = {
    val exams = Exam.findAll().filter(_.onLine.get).filter(_.started_?).filter(!_.expired_?)
    "#exam *" #> exams.map {
      exam => ".exam-name *" #> exam.name.get &
        ".exam-descp *" #> exam.description.get &
        ".exam-timeleft *" #> Text(TimeSpan.format((exam.dateOfExpired.get.getTime - now.getTime) / 1000L * 1000L).split(",").head) &
        "a" #> SHtml.link("/public/doexam/%s".format(exam.id.get), () => selectedExam.set(Full(exam)), Text("Go"), ("class" -> "btn btn-large btn-primary"))
    }
  }
}
