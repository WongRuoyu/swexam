package com.wong
package model

import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.common.{Empty, Box}
import net.liftweb.sitemap.Loc.AnyLocParam
import scala.xml._
import net.liftweb.common.Full
import net.liftweb.sitemap.Loc.LocGroup
import net.liftweb.http.SHtml

/**
 * Created by Wong on 15-8-2.
 */

class Question extends LongKeyedMapper[Question] with CreatedUpdated with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, Question] = Question

  override def asHtml: NodeSeq = {
    val options = this.options.get.split(",")
    val optsHtml: Array[Elem] = this.questionType.get match {
      case QuestionType.single => options.map {
        option => <label class="radio">
          <input type="radio" name={this.id.get.toString} value={option}></input>{option}
        </label>
      }
      case QuestionType.multiC => options.map {
        option => <label class="checkbox">
          <input type="checkbox" name={this.id.get.toString} value={option}></input>{option}
        </label>
      }
      case QuestionType.rightOw => options.map {
        option => <label class="radio">
          <input type="radio" name={this.id.get.toString} value={option}></input>{option}
        </label>
      }
      case _ => Array(<div>options no found</div>)
    }
    val questionHtml = <div class="question">
      {this.content.asHtml}<div class="question-options" id={this.id.get.toString}>
        {optsHtml.flatten}
      </div>
    </div>

    questionHtml
  }

  //问题的内容
  object content extends MappedString(this, 1024) {

    //用于在toForm调用时，返回字段的标签
    override def displayName: String = "内容"

    override def validations = contentUnique("question already exists!") _ :: super.validations

    override def asHtml: Node = <div class="question-content">
      {this.get.toString}
    </div>

    //为form元素添加html属性
    override def formElemAttrs: Seq[SHtml.ElemAttr] = Seq(("classs" -> "control-group"))

    /*    override def toForm: Box[Elem] = Full(
          <div class="control-group">
            <label class="control-label" for="content">
              Content
            </label>
            <div>
              <input id="content" type="text" placeholder="input the content" value={this.get}/>
            </div>
          </div>
        )*/

  }

  private def contentUnique(errorMsg: => String)(content: String): List[FieldError] = Question.unique_?(content) match {
    case true => Nil
    case false => FieldError(this.content, errorMsg) :: Nil
  }

  object questionType extends MappedEnum(this, QuestionType) {
    //生成form时该字段不显示，即无法修改
    override def dbIncludeInForm_? : Boolean = false
  }

  object points extends MappedInt(this) {
    override def defaultValue = 0

    /*    override def toForm: Box[Elem] = Full(
          <div class="control-group">
            <label class="control-label" for="points">
              Points
            </label>
            <div>
              <input id="points" type="text" placeholder="input the points" value={this.get.toString}/>
            </div>
          </div>
        )*/
  }

  object options extends MappedString(this, 2048) {
    /*    override def toForm: Box[Elem] = Full(
          <div class="control-group">
            <label class="control-label" for="options">
              options
            </label>
            <div>
              <input id="options" type="text" value={this.get} placeholder="input the options"/>
            </div>
          </div>
        )*/
  }

  //错误的次数
  object wrongTimes extends MappedInt(this) {
    override def defaultValue = 0

    override def dbIncludeInForm_? : Boolean = false
  }

  //问题的答案，on or more of (A|B|C|D)
  object answer extends MappedString(this, 32) {
    /*    override def toForm: Box[Elem] = Full(
          <div class="control-group">
            <label class="control-label" for="answer">
              Answer
            </label>
            <div>
              <input id="answer" type="text" value={this.get}></input>
            </div>
          </div>
        )*/
  }

  //问题所对应的知识点，多对多
  //HasManyThrough为懒加载，每个请求只加载一次
  // object knowledgepts extends HasManyThrough(this, KnowledgePoint, QuestionKnowledgepts, QuestionKnowledgepts.knowledgept, QuestionKnowledgepts.question)

  lazy val numKpts = knowledgepts.size

  def knowledgepts: List[KnowledgePoint] = QuestionKnowledgepts.findAll(By(QuestionKnowledgepts.question, this.id.get)).map(_.knowledgept.obj.map(kp => kp)).flatten

  // object knowledgepts extends MappedManyToMany(QuestionKnowledgepts, QuestionKnowledgepts.question, QuestionKnowledgepts.knowledgept, KnowledgePoint)

  //问题的创建者
  // object creator extends MappedLongForeignKey(this, Teacher)
  // object creator extends MappedManyToMany()

  //所有打错此题的user
  object wrongers extends HasManyThrough(this, User, UserQuestions, UserQuestions.user, UserQuestions.question)

  //无效
  override def toForm(button: Box[String], redoSnippet: (NodeSeq) => NodeSeq, onSuccess: (Question) => Unit): NodeSeq =
  //      <lift:field name="content"/>
    <div class="form-horizontal">
      <div class="control-group">
        {this.content.toForm}
      </div>
      <div class="control-group">
        {this.options.toForm}
      </div>
      <div class="control-group">
        {this.points.toForm}
      </div>
    </div>
}

object Question extends Question with LongKeyedMetaMapper[Question] with CRUDify[Long, Question] {
  override def dbTableName = "question"

  //将Question对象的操作设定为admin组
  override def showAllMenuLocParams: List[AnyLocParam] = List(LocGroup("admin"))

  //定义数据库初始化(添加数据表)时所进行的操作
  override def dbAddTable = Full(populate _)

  private def populate {
    val qNames = "question1" :: "question2" :: "question3" :: "question4" :: Nil
    for (q_name <- qNames) {
      Question.create.content(q_name).save
    }
  }

  private def unique_?(content: String): Boolean = Question.find(By(Question.content, content)).isEmpty

  case class QuestionKp(val q: Question, val kp: KnowledgePoint)

  //从文件中的一行中创建Question实体
  def fromString(src: String): Box[QuestionKp] = {
    //    println("src:" + src);
    //    println(src.split(",").length)
    src.split(",") match {
      case Array(content, type1, ans, opA, opB, opC, opD, opE, opF, points, kp) => {
        val _type: QuestionType.Value = type1 match {
          case "单选题" => QuestionType.single
          case "多选题" => QuestionType.multiC
          case "判断题" => QuestionType.rightOw
          case _ => QuestionType.single
        }
        val options = opA + "," + opB + "," + opC + "," + opD + "," + opE + "," + opF
        //        println("options:" + options.trim)
        val q = Question.create.content(content).options(options.trim).answer(ans).points(points.toInt).questionType(_type)
        //        println("content:" + content)
        //        println("ans:" + ans)
        val _kp = KnowledgePoint.find(By(KnowledgePoint.name, kp)) openOr KnowledgePoint.create.name(kp)
        Full(QuestionKp(q, _kp))
      }
      case _ => Empty
    }
  }
}

object QuestionType extends Enumeration {
  val single = Value(0, "sigleChoise")
  val multiC = Value(1, "multChoise")
  val rightOw = Value(2, "rightOrwrong")
}