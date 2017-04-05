package com.wong
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.sitemap.Loc._
import net.liftmodules.FoBoBs.mapper._
import net.liftweb.sitemap.Menu
import scala.xml.NodeSeq

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] with OneToMany[Long, User] {
  // what's the "meta" server
  def getSingleton = User

  object wrongQuestions extends HasManyThrough(this, Question, UserQuestions, UserQuestions.question, UserQuestions.user)

  def finishExam(exam: Exam) {
    ExamAssignment.findAll(By(ExamAssignment.exam, exam.id.get), By(ExamAssignment.user, this.id.get))
      .map(_.done(true).save())
  }

  def finishExamById(examID: Long) {
    ExamAssignment.findAll(By(ExamAssignment.exam, examID)).map(_.done(true).save())
  }

  def examHists: List[ExamHistory] = ExamHistory.findAll(By(ExamHistory.user, this.id.get))

  //该用户为完成的考试
  def examsTofinish: List[ExamAssignment] =
    ExamAssignment.findAll(By(ExamAssignment.user, this.id.get), By(ExamAssignment.done, false))
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] with BootstrapMegaMetaProtoUser[User] {
  // define the DB table name
  override def dbTableName = "users"

  override def screenWrap = Full(<lift:surround with="default" at="content">
    <lift:bind/>
  </lift:surround>)

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, password)

  override def firstNameDisplayName: String = "姓名"

  //the fields for signup
  override def signupFields: List[User.FieldPointerType] = email :: firstName :: password :: Nil

  //自定义的用户注册验证逻辑。我们不需要验证email，只验证用户名不重复即可
  override def validateSignup(user: User.TheUserType): List[FieldError] = this.find(By(this.firstName, user.firstName.get)) match {
    case Full(user) => FieldError(this.firstName, "User already exist!") :: Nil
    case Empty => Nil
  }

  override def editFields: List[User.FieldPointerType] = firstName :: password :: Nil

  /*  override def loginXhtml =
      <form method="POST" action={S.uri} class="form-horizontal">
        <div class="control-group">
          <label class="control-label">用户名</label>
          <div class="controls">
            <user:first-Name/>
            <input type="text" placeholder="username"></input>
          </div>
        </div>
        <div class="control-group">
          <label class="control-label">密码</label>
          <div class="controls">
            <user:password/>
            <input type="text" placeholder="password"></input>
          </div>
        </div>
        <div class="control-group">
          <user:submit/>
          <button type="submit" class="btn btn-primary btn-large">Loggin</button>
        </div>
      </form>*/


  //不进行邮箱验证skipEmailValidation
  override def skipEmailValidation = true

  //add a loc group to the user menu
  override def globalUserLocParams: List[LocParam[Unit]] = List(LocGroup("user"))

  override def resetPasswordMenuLoc: Box[net.liftweb.sitemap.Menu] = Box(Empty)

  override def validateUserMenuLoc: Box[net.liftweb.sitemap.Menu] = Box(Empty)

  //LostPassword丢失密码的链接
  override def lostPasswordMenuLoc: Box[Menu] = Box(Empty)

  override def dbAddTable: Box[() => Unit] = Full(populate _)

  private def populate {
    User.firstName("tuser1").password("123").save
  }
}



