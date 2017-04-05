package com.wong
package model

import net.liftweb.mapper._

/**
 * Created by Wong on 15-8-2.
 * 用户与所做错题的关联
 */
class UserQuestions extends LongKeyedMapper[UserQuestions] with CreatedUpdated with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, UserQuestions] = UserQuestions

  object user extends MappedLongForeignKey(this, User) {
    override def dbColumnName = "user_id"
  }

  object question extends MappedLongForeignKey(this, Question) {
    override def dbColumnName = "question_id"
  }

  object doRight extends MappedBoolean(this) {
    override def dbColumnName = "result"

    override def defaultValue: Boolean = false
  }

}

object UserQuestions extends UserQuestions with LongKeyedMetaMapper[UserQuestions] {
  def join(user: User, question: Question, re: Boolean) =
    this.create.user(user).question(question).doRight(re).save()

}
