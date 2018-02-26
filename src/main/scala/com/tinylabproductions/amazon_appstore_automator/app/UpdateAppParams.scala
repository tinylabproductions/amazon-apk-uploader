package com.tinylabproductions.amazon_appstore_automator.app

case class UpdateAppParams(
  submitApp: Boolean, appDirectedUnderAge13: Option[Boolean]
) {
  override def toString: String = {
    import UpdateAppParams._
    s"""Update application options:
       |- Submit application: $submitApp
       |- App directed to kids under age 13: ${asString(appDirectedUnderAge13)}
     """.stripMargin
  }
}
object UpdateAppParams {
  def asString[A](opt: Option[A]): String = opt.fold("not changed")(_.toString)
}
