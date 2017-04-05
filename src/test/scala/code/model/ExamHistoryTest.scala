package com.wong
package test.model

import org.specs2.mutable.Specification
import com.wong.model.ExamHistory
import net.liftweb.mapper.By
import net.liftweb.common.Empty

/**
 * Created by Wong on 15-8-4.
 */
class ExamHistoryTest extends Specification with initDB {
  "The ExamHistory Entity" should {
    "Fetch records" in {
      "by user_id" in {
        initDB
        val user_id = 3
        val exams = ExamHistory.findAll(By(ExamHistory.user, user_id))
        exams.size must_== (3)
      }
      "by exam_id from db" in {

        val exam_id = 9
        val exams = ExamHistory.findAll(By(ExamHistory.exam, exam_id))
        exams.size must_== (3)
      }
      "by id and exam_id" in {
        val id = 12
        val exam_id = 9
        val examHist = ExamHistory.findAll(By(ExamHistory.id, id), By(ExamHistory.exam, exam_id))
        examHist.map(_.total_points).contains(11) must beTrue
      }
    }
  }
}
