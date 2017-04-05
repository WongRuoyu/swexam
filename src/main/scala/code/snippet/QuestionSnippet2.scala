package com.wong
package snippet

import net.liftweb.http.{S, StatefulSnippet}
import net.liftweb.http.SHtml._

import com.wong.model.KnowledgePoint
import net.liftweb.util.Helpers._
import net.liftweb.mapper.By
import scala.xml.Text
import net.liftweb.http.js.JsCmds.Run

/**
 * Created by Wong on 15-8-2.
 */
class QuestionSnippet2 extends StatefulSnippet with QuestionSptHelper {
  override def dispatch: DispatchIt = {
    case "opts" => opts
    case "indicate" => indicate

  }

  def indicate = {
    val kpts = KnowledgePoint.findAll()
    ".kpts" #> kpts.map("li [id]" #> _.id.get.toString)
  }

  def opts = {
    val id = S.param("kp_id").openOr(0L)
    val cmd = "$('#%s').parent('li').addClass('active')".format(id)
    //Run对象只是生成js代码而不负责运行，S.appendJs则是将代码插入页面，从而运行代码
    S.appendJs(Run(cmd))
    val kpts = KnowledgePoint.findAll()
    ".kpts *" #> kpts.map(
      kpt => a(() => filt(kpt.name.toString), Text(kpt.name.toString), ("id" -> kpt.id.get.toString))
    ).::(a(() => filt("All"), Text("All"), "id" -> "0"))
  }

  private def filt(kpt: String) = {
    val kp = KnowledgePoint.find(By(KnowledgePoint.name, kpt))
    redirectTo("/question/filter/" + kp.map(_.id).openOr(0L))
  }
}
