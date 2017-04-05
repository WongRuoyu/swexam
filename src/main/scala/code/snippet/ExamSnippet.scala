package com.wong
package snippet


import net.liftweb.http._
import com.wong.model._
import net.liftweb.common.{Full, Box, Empty}
import scala.xml.{XML, Text, NodeSeq}
import net.liftweb.util.Helpers._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.{SetHtml, Run}
import net.liftweb.http.SHtml.ChoiceHolder
import net.liftweb.common.Full
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.mapper.By

/**
 * Created by Wong on 15-8-2.
 */
class ExamSnippet extends StatefulSnippet with ExamHelper {
  override def dispatch: DispatchIt = {
    case "add" => add
    case "allExams" => allExams
    case "currentContent" => currentContent
    case "editButton" => editButton
    case "allQuestions" => allQuestions
    case "examPreview" => examPreview
    case "assignStudents" => assignStudents
    case "examResults" => examResults
  }

  def examResults = {
    (for (exam <- selectedExam.is) yield {
      val results = ExamHistory.findAll(By(ExamHistory.exam, exam.id.get))
        .map(examHist => (examHist.user.get, examHist.total_points.get, examHist.createdAt.get))
        .map {
        examHist =>
          val students = User.findAll(By(User.id, examHist._1))
            .map(user => (user.firstName.get, examHist._3, examHist._2))
          students
      }.toList.flatten
      //      println("results is: " + results)
      "#exam_results *" #> results.map {
        result => ".student_name *" #> result._1 &
          ".student_date *" #> result._2.toString &
          ".student_score *" #> result._3
      }
    }) openOr {
      S.redirectTo("/exam/exams.html")
    }
  }

  def assignStudents = {
    val students = User.findAll()
    val options: List[(User, String)] = students.map(student => (student -> student.firstName.get))
    val default = students.head :: Nil
    (for (exam <- selectedExam.is) yield {
      def assignStudents(students: List[User]): Unit = {
        students.foreach(ExamAssignment.join(User.currentUser.openOr(User.create), _, exam))
      }
      "#students *" #> SHtml.multiSelectObj[User](options, default, assignStudents, ("multiple" -> "multiple"))
    }) openOr {
      S.redirectTo("/exam/exams.html")
    }
  }


  def examPreview = "#exam *" #> selectedExam.is.map {
    exam =>
      ".exam-title *" #> exam.name.get &
        ".exam-descp" #> exam.description.get &
        "#questions" #> XML.loadString("<div class='questions'>" + exam.content.get + "</div>")
  }

  // 方案三：finished，使用checkbox
  def allQuestions = {
    (for (exam <- selectedExam.is) yield {
      val options = Question.findAll()
      val default = Seq(options.head)
      var selections = default
      def onSubmit = {
        val contents = selections.map(_.asHtml.mkString("")).reduceLeft(_ + _)
        //        println("contents: " + contents)
        exam.content(contents).save()
        //        println("exam:" + exam)
        val html = selections.map(_.asHtml).reduceLeft(_ ++ _)
        //ajaxSubmit，不需要刷新页面
        //          JsCmds.Reload
        SetHtml("content", html)
      }
      val checkboxes: ChoiceHolder[Question] = SHtml.checkbox[Question](options.toSeq, default, selections = _, ("class" -> "question-checker"))

      "#all-questions *" #> checkboxes.map {
        holder => ".checker *" #> holder.xhtml &
          ".question-content *" #> holder.key.content.get
      } &
        "input" #> SHtml.ajaxSubmit("OK!", () => onSubmit)
    }) openOr {
      S.notice("no exam found!");
      S.redirectTo("/exam/exams.html")
    }
  }

  // 方案二：finished，方案一添加ajaxbutton
  /*  def allQuestions = {
      (for (exam <- selectedExam.is) yield {
        def addQuesions(questions: List[Question]) {
          val contents = questions.map(_.asHtml.mkString("")).reduceLeft(_ + _)
          exam.content(contents).save()
        }
        val questions = Question.findAll()
        val options: List[(Question, String)] = questions.map(question => (question -> question.content.get.toString))
        val default = questions.head :: Nil
        var selections = default
        def onSubmit = {
          val contents = selections.map(_.asHtml.mkString("")).reduceLeft(_ + _)
          //        println("contents: " + contents)
          exam.content(contents).save()
          //        println("exam:" + exam)
          val html = selections.map(_.asHtml).reduceLeft(_ ++ _)
          //ajaxSubmit，不需要刷新页面
          //          JsCmds.Reload
          SetHtml("content", html)
        }

        "#all-questions" #> SHtml.multiSelectObj[Question](options, default, selections = _, ("height" -> "60%"), ("width" -> "300px")) &
          "input" #> SHtml.ajaxSubmit("OK!", () => onSubmit)
      }) openOr {
        S.notice("no exam found!");
        S.redirectTo("/exam/exams.html")
      }
    }*/

  def editButton = {
    lazy val showModal: JsCmd = Run("alert('hello')")
    //    lazy val showModal: JsCmd = Run("$('#all-questions-Modal').modal('show')")
    "button [onclick]" #> "$('#all-questions-Modal').modal('show')"
  }

  object selectedExam extends RequestVar[Box[Exam]](Empty)

  def currentContent = "#content" #> selectedExam.is.map(_.content.get).openOr("No contents")

  //渲染考试列表
  def allExams = {
    val exams = Exam.findAll()
    "#all_exams *" #> exams.map {
      exam => singal(exam) & asEditLink(exam) & asPublishButton(exam) &
        asPreviewButton(exam) & asAssignButton(exam) & asResultButton(exam)
    }
  }

  private def asResultButton(exam: Exam) = ".exam_results *" #> SHtml.link("/exam/result",
    () => selectedExam.set(Full(exam)), Text("View Results"), "class" -> "btn btn-link")


  private def asAssignButton(exam: Exam) = ".assign *" #> SHtml.link("/exam/assign",
    () => selectedExam.set(Full(exam)), Text("AssignStudents"), "class" -> "btn btn-link")

  private def asPreviewButton(exam: Exam) = if (exam.content.get != null) {
    ".preview *" #> SHtml.link("/exam/preview", () => {
      selectedExam.set(Full(exam));
    }, Text("Preview"), "class" -> "btn btn-link")
  }
  else {
    ".preview *" #> "试卷内容为空"
  }

  private def asEditLink(exam: Exam) = if (exam.editable_?) {
    ".edit *" #> SHtml.link("/exam/edit", () => {
      selectedExam.set(Full(exam));
    }, Text("Edit"), "class" -> "btn btn-link")
  } else {
    ".edit *" #> SHtml.link("/exam/edit", () => selectedExam.set(Full(exam)), Text("Edit"), "class" -> "btn btn-small disabled")
  }


  private def asPublishButton(exam: Exam) = if (exam.publishable_?) {
    ".publish *" #> SHtml.ajaxButton("Publish it", {
      () =>
        selectedExam.set(Full(exam));
        selectedExam.is.map(_.onLine(true).save());
        //        JsCmds.Alert("exam published!");
        //发布后重新刷新页面，否则按钮样式不改变
        JsCmds.Reload
    }, "class" -> "btn btn-primary")
  }
  else {
    ".publish *" #> "无法发布"
  }

  /*  private def asPublishButton(exam: Exam) = (exam.publishable_?, exam.onLine.get) match {
      case (true, false) => ".publish *" #> SHtml.ajaxButton("Publish it", {
        () =>
          selectedExam.set(Full(exam));
          selectedExam.is.map(_.onLine(true).save());
          //        JsCmds.Alert("exam published!");
          //发布后重新刷新页面，否则按钮样式不改变
          JsCmds.Reload
      }, "class" -> "btn btn-primary")
      case (false, _) => ".publish *" #> "无法发布"
      case (_, true) => ".publish *" #> "已发布"
    }*/

  //  lazy val enableDatePicker = Run("$('.timestamp').datetimebox({showSeconds: false});")

  def add(in: NodeSeq): NodeSeq = {
    //    S.appendJs(enableDatePicker)
    selectedExam.is.openOr(new Exam).toForm(Empty, saveExam _) ++
      <div>
        <div>
          <a href="/index.html">
            back
          </a>
        </div>
        <div>
          <input type="submit" value="Create "/>
        </div>
      </div>
  }

  private def saveExam(exam: Exam) = exam.validate match {
    case Nil => exam.save();
      S.redirectTo("/exam/exams.html")
    case error => selectedExam.set(Full(exam));
      S.error("examerr", "exam validation failed!")
  }
}

trait ExamHelper {
  def singal(exam: Exam) = ".name *" #> exam.name.get &
    ".descpt *" #> exam.description.get &
    ".create_date *" #> exam.dateOfCreated.get.toString &
    ".start_date *" #> exam.dateOfStart.get.toString &
    ".expired_date *" #> exam.dateOfExpired.get.toString
}
