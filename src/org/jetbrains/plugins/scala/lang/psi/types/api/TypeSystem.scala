package org.jetbrains.plugins.scala.lang.psi.types.api

/**
  * @author adkozlov
  */
trait TypeSystem {
  val name: String
  val equivalence: Equivalence
}

trait TypeSystemOwner {
  implicit val typeSystem: TypeSystem
}
