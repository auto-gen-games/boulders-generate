import mill._
import mill.scalalib._

object boulders extends ScalaModule {
  def scalaVersion = "2.12.4"
  override def mainClass = Some ("org.drsimonmiles.rocks.Main")
  def ivyDeps = Agg (
    ivy"io.circe::circe-core:0.12.3",
    ivy"io.circe::circe-generic:0.12.3",
    ivy"io.circe::circe-parser:0.12.3"
  )
}
