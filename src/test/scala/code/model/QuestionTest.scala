package com.wong
package test.model

import com.wong.model.Exam
import org.specs2.mutable.Specification
import net.liftweb.mapper.By
import net.liftweb.common.Empty

/**
 * Created by Wong on 15-8-3.
 */
object QuestionTestSpecs extends Specification with initDB {


  "The Exam Enity" should {
    "Fetch a record" in {
      initDB
      val exam = Exam.find(By(Exam.name, "new exam"))
      exam mustEqual Empty
    }
  }
}
