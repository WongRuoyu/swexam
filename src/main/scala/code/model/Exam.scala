package com.wong
package model

import net.liftweb.mapper._
import scala.xml.{NodeSeq, Text}
import net.liftweb.common.{Box, Full}
import java.text.DateFormat
import java.util.Date
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldError
import net.liftweb.http.SHtml

/**
  * Created by Wong on 15-8-12.
  */
class Exam extends LongKeyedMapper[Exam] with CreatedUpdated with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, Exam] = Exam

  //该试卷所分配的用户
  val students = ExamAssignment.findAll(By(ExamAssignment.exam, this.id.get))

  object name extends MappedString(this, 1024) {
    override def defaultValue: String = "new exam"

    override def validations = nameUnique("the name has exist!") _ :: super.validations
  }

  //存储试卷的内容，包含该试卷中所有问题的Id，问题的内容，问题的分数，问题的知识点
  object content extends MappedText(this) {
    override def dbIncludeInForm_? : Boolean = false
  }

  //总分数
  object points extends MappedInt(this)

  object description extends MappedString(this, 2048) {
    override def defaultValue: String = "no descts"
  }

  object duration extends MappedInt(this) {
    override def defaultValue: Int = 60

    override def displayName = "duration (in minutes)"
  }

  //是否已上线，用户只能参加已上线的考试
  object onLine extends MappedBoolean(this) {
    override def defaultValue = false

    override def dbIncludeInForm_? = false
  }

  lazy val editable_? = !this.onLine.get

  lazy val expired_? = this.dateOfExpired.get.before(now)

  lazy val started_? = this.dateOfStart.get.before(now)
  //未上线且内容不为空，才可被上线
  lazy val publishable_? = (!this.onLine.get) && (this.content.get != null)

  object dateOfCreated extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM)

    override def dbIncludeInForm_? = false

    override def defaultValue = now

    override def required_? = true

    override def asHtml = Text(dateFormat.format(this.get))

    override def parse(s: String): Box[Date] = Full(tryo(dateFormat.parse(s)) getOrElse now)

    override def formElemAttrs: Seq[SHtml.ElemAttr] = Seq("class" -> "timestap")
  }

  object dateOfExpired extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM)

    override def defaultValue = now

    override def required_? = true

    override def asHtml = Text(dateFormat.format(this.get))

    override def parse(s: String): Box[Date] = Full(tryo(dateFormat.parse(s)) getOrElse now)

    override def formElemAttrs: Seq[SHtml.ElemAttr] = Seq("class" -> "timestamp")
  }

  object dateOfStart extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM)

    override def defaultValue = now

    override def required_? = true

    override def asHtml = Text(dateFormat.format(this.get))

    override def parse(s: String): Box[Date] = Full(tryo(dateFormat.parse(s)) getOrElse now)

    override def formElemAttrs: Seq[SHtml.ElemAttr] = Seq("class" -> "timestamp")
  }

  private def nameUnique(errorMsg: => String)(name: String): List[FieldError] = Exam.unique_?(name.trim) match {
    case true => Nil
    case false => FieldError(this.name, errorMsg) :: Nil
  }
}

object Exam extends Exam with LongKeyedMetaMapper[Exam] {
  override def dbTableName = "exams"

  private def unique_?(name: String) = Exam.findAll(By(Exam.name, name)).isEmpty

  override def dbAddTable: Box[() => Unit] = Full(initTable)

  private def initTable() {
    val names = "exams" :: "exam2" :: "exam3" :: Nil
    for (name <- names) {
      Exam.create.name(name).save()
    }
  }
}