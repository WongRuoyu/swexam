package com.wong
package test.model

import net.liftweb.mapper.DB
import net.liftweb.db.StandardDBVendor
import net.liftweb.util.Props
import net.liftweb.http.LiftRules
import net.liftweb.util

/**
 * Created by Wong on 15-8-4.
 */
trait initDB {

  def initDB = {
    if (!DB.jndiJdbcConnAvailable_?) {
      sys.props.put("h2.implicitRelativePath", "true")
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }
  }
}
