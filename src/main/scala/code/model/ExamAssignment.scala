package com.wong
package model

import net.liftweb.mapper._
import net.liftweb.util.FieldError

/**
  * Created by Wong on 15-8-4.
  * 指派用户与考试之间的关联，提醒用户完成考试
  */
class ExamAssignment extends LongKeyedMapper[ExamAssignment] with CreatedUpdated with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, ExamAssignment] = ExamAssignment


  object creator extends MappedLongForeignKey(this, User) {
    override def dbColumnName = "creator_id"

    override def dbNotNull_? = false
  }

  object user extends MappedLongForeignKey(this, User) {
    override def dbColumnName: String = "user_id"

    override def dbNotNull_? : Boolean = true
  }

  object exam extends MappedLongForeignKey(this, Exam) {
    override def dbColumnName: String = "exam_id"

    override def dbNotNull_? : Boolean = true
  }

  object done_? extends MappedBoolean(this) {
    override def defaultValue = false
  }

  override def validate: List[FieldError] = ExamAssignment.entityUniqueValidation(this.user.get, this.exam.get) match {
    case Nil => Nil
    case _ => FieldError(this.user, "this user has been assigned to this exam!") :: FieldError(this.exam, "this exam has been assigned to this user!") :: Nil
  }
}

object ExamAssignment extends ExamAssignment with LongKeyedMetaMapper[ExamAssignment] {

  override def dbName: String = "exam_assignments"

  private def entityUniqueValidation(user: Long, exam: Long): List[ExamAssignment] =
    ExamAssignment.findAll(By(ExamAssignment.user, user), By(ExamAssignment.exam, exam))

  def join(creator: User, user: User, exam: Exam) = {
    val assignment = ExamAssignment.create.creator(creator).user(user).exam(exam).done_?(false)
    assignment.validate match {
      case Nil => assignment.save()
      case _ => false
    }
  }

}
