package com.wong
package model

import net.liftweb.mapper._
import net.liftweb.common.Full

/**
 * Created by Wong on 15-8-2.
 * 知识点
 */

class KnowledgePoint extends LongKeyedMapper[KnowledgePoint] with CreatedUpdated with IdPK with ManyToMany {
  override def getSingleton: KeyedMetaMapper[Long, KnowledgePoint] = KnowledgePoint

  object name extends MappedString(this, 64) {
    override def validations = valMaxLen(64, "the name is too long!") _ :: super.validations
  }

  object description extends MappedText(this)

  object questions extends HasManyThrough(this, Question, QuestionKnowledgepts, QuestionKnowledgepts.question, QuestionKnowledgepts.knowledgept)

  lazy val numQsts = questions.get.size
  //  val questions = QuestionKnowledgepts.findAll(By(QuestionKnowledgepts.knowledgept, this.id)).map(_.question.obj.map(q => q)).flatten

  //  object questions extends MappedManyToMany(QuestionKnowledgepts, QuestionKnowledgepts.knowledgept, QuestionKnowledgepts.question, KnowledgePoint)

}

object KnowledgePoint extends KnowledgePoint with LongKeyedMetaMapper[KnowledgePoint] with CRUDify[Long, KnowledgePoint] {

  override def dbAddTable = Full(populate _)

  private def populate {
    val names = "kp1" :: "kp2" :: "kp3" :: Nil
    val descts = "desc1" :: "desc2" :: "desc3" :: Nil
    for {name <- names
         desct <- descts} {
      KnowledgePoint.create.name(name).description(desct).save
    }
  }
}


