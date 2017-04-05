package com.wong
package snippet

import net.liftweb.http.{OnDiskFileParamHolder, S, FileParamHolder, SHtml}
import net.liftweb.common.{Loggable, Full, Empty, Box}
import net.liftweb.util.Helpers._
import scala.io.Source
import com.wong.model.{KnowledgePoint, QuestionKnowledgepts, Question}
import com.wong.model.Question.QuestionKp
import scala.xml.Attribute

/**
 * Created by Wong on 15-8-15.
 */
class QuestionUpload extends Loggable {
  var fileHolder: Box[FileParamHolder] = Empty

  def processForm() = fileHolder match {
    case Full(content: OnDiskFileParamHolder) if content.fileName.endsWith(".csv") =>
      this.logger.info("file: " + content.fileName + " saved")
      this.processFile(content)
    case _ => S.notice("uploadMsgs", "File upload failed..")
  }

  private def processFile(content: OnDiskFileParamHolder) = {
    val file = Source.fromFile(content.localFile)
    var i = 0
    for (line <- file.getLines()) {
      i = i + 1
      processLine(line, i)
    }
  }

  private def processLine(line: String, i: Int) = Question.fromString(line) match {
    case Full(QuestionKp(question, kp)) => {
      question.validate match {
        case Nil =>
          KnowledgePoint.save(kp)
          Question.save(question)
          QuestionKnowledgepts.join(question, kp)
          S.notice("uploadMsgs", <div>
            <a href={"show/" + question.id.toString}>
              {"Question " + question.id.toString()}
              创建成功
            </a>
          </div>)
        case error => S.error("uploadMsgs", <p class="text-error">
          {"第" + i + "行:" + error.head.msg}
        </p>)
      }
    }
    case _ => S.error("uploadMsgs", "第" + i + "行无法构成实体。")
  }

  def upload = {
    "#file" #> SHtml.fileUpload(f => fileHolder = Full(f)) &
      "type=submit" #> SHtml.onSubmitUnit(processForm)
  }
}
