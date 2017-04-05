package com.wong
package model

import net.liftweb.mapper._
import net.liftweb.common.Full

/**
 * Created by Wong on 15-8-3.
 */
class QuestionKnowledgepts extends LongKeyedMapper[QuestionKnowledgepts] with IdPK {
  override def getSingleton: KeyedMetaMapper[Long, QuestionKnowledgepts] = QuestionKnowledgepts

  object question extends MappedLongForeignKey(this, Question)

  object knowledgept extends MappedLongForeignKey(this, KnowledgePoint)

}

object QuestionKnowledgepts extends QuestionKnowledgepts with LongKeyedMetaMapper[QuestionKnowledgepts] {
  def join(question: Question, pt: KnowledgePoint) = {
    this.create.question(question).knowledgept(pt).save()
  }

  /*  override def dbAddTable = Full(populate _)

    private def populate {
      val questions = Question.findAll()
      val kps = KnowledgePoint.findAll()
      for {q <- questions
           pt <- kps} {
        this.join(q, pt)
      }
    }*/
}