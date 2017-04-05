package com.wong
package snippet

import com.wong.model.{UserQuestions, Exam, User}
import scala.xml.Text
import net.liftweb.util.Helpers._
import net.liftweb.mapper.By
import net.liftweb.common.Full
import java.util.Date

/**
 * Created by Wong on 15-8-6.
 */
class UserSnippet {
  def username = User.currentUser match {
    case Full(user) => "#username *" #> user.firstName.toString
    case _ => "#username *" #> "no_user"
  }

  def numTodos = "#number *" #> User.currentUser.map(_.examsTofinish).toList.flatten.size

  def todoExam = {
    val infoes = User.currentUser.map(_.examsTofinish).toList.flatten
      .map(ea => (ea.exam.get, ea.createdAt.get, ea.creator.get)).map {
      tp =>
        val exam = Exam.find(By(Exam.id, tp._1))
        val examName = exam.map(_.name.get).openOr("name not known")
        val expired = exam.map(_.dateOfExpired.get).openOr(now)
        val creatorName = User.find(By(User.id, tp._3)).map(_.firstName.get).openOr("no creator name")
        //(id,exam_name,expireDate,exam_assignment_creatdAt,creatorName)
        (tp._1, examName, expired, tp._2, creatorName)
    }
    def time_left(date: Date) = TimeSpan.format((date.getTime - now.getTime) / 1000L * 1000L).split(",").take(2).mkString(",")

    "#exam_todo *" #> infoes.map {
      info => "#exam_toname *" #> info._2 &
        "#exam_creator *" #> info._5 &
        "#exam_assigndate *" #> info._4.toString &
        "#exam_due *" #> time_left(info._3) &
        "a [href]" #> "/public/doexam/%s".format(info._1)
    }
  }

  def examHist = {
    val exams = User.currentUser.map(_.examHists).toList.flatten
      .map(examHist => (examHist.exam.get, examHist.createdAt.get, examHist.total_points.get))
      .map {
      examHist =>
        val examName = Exam.find(By(Exam.id, examHist._1)).map(_.name.get).openOr("no names")
        (examName, examHist._2, examHist._3)
    }

    "#exam_hists *" #> exams.map {
      exam => "#exam_id *" #> exam._1 &
        "#exam_date *" #> exam._2.toString &
        "#exam_score *" #> exam._3
    }
  }

  def questionHist = {
    val questions =
      UserQuestions.findAll(By(UserQuestions.user, User.currentUser.map(_.id.get).openOr(0L))).map(uq => (uq.question, uq.createdAt))
    "#questions_details *" #> questions.map {
      uq =>
        "#question_id *" #> uq._1 &
          "#question_date *" #> uq._2.get.toString
    }
  }
}
