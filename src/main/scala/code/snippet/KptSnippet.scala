package com.wong
package snippet

import com.wong.model.KnowledgePoint
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml

/**
 * Created by Wong on 15-8-6.
 */
class KptSnippet {
  lazy val allKpts = KnowledgePoint.findAll()

  def all = "#kpts" #> {
    allKpts.map {
      kpt =>
        "a *" #> kpt.name
    }
  }

}
