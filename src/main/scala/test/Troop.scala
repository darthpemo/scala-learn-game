package test


sealed trait TroopType {
  val name: String
  val attacksAt: Int
  val defendsAt: Int
}

