import mill._
import mill.scalalib._

object rocks extends ScalaModule {
  def scalaVersion = "2.12.4"
  override def mainClass = Some ("org.drsimonmiles.rocks.Create")
}
