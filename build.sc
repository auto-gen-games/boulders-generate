import mill._
import mill.scalalib._

object boulders extends ScalaModule {
  def scalaVersion = "2.13.1"
  override def mainClass = Some ("org.drsimonmiles.rocks.Main")
  def ivyDeps = Agg (
    ivy"io.circe::circe-core:0.13.0",
    ivy"io.circe::circe-generic:0.13.0",
    ivy"io.circe::circe-parser:0.13.0"
  )

  object test extends Tests {
    def ivyDeps = Agg (
      ivy"com.lihaoyi::utest::0.7.5"
    )
    def testFrameworks = Seq ("utest.runner.Framework")
  }
}

/*
com.lihaoyi:utest_2.13:0.7.5
io.circe:circe-core_2.13:0.13.0
io.circe:circe-generic_2.13:0.13.0
io.circe:circe-parser_2.13:0.13.0
 */