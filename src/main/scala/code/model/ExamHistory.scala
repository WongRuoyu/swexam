package com.wong
package model

import net.liftweb.mapper._

/**
 * Created by Wong on 15-8-1.
 */
class ExamHistory extends LongKeyedMapper[ExamHistory] with CreatedUpdated with IdPK {
  override def getSingleton = ExamHistory

  object user extends MappedLongForeignKey(this, User) {
    override def dbColumnName: String = "user_id"
  }

  object exam extends MappedLongForeignKey(this, Exam) {
    override def dbColumnName: String = "exam_id"
  }

  object total_points extends MappedDouble(this)

  object answers extends MappedText(this)

}

object ExamHistory extends ExamHistory with LongKeyedMetaMapper[ExamHistory] {
  def join(user: User, exam: Exam, points: Double, answer: String) =
    this.create.user(user).exam(exam).total_points(points).answers(answer).saveMe()
}
