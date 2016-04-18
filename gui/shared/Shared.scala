package shared



trait Api {
  case class ModelState()

  def uuid(): String = java.util.UUID.randomUUID.toString
}