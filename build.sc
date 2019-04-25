import mill._
import mill.scalalib._
import mill.util.Loose

object rocks extends ScalaModule {
  def scalaVersion = "2.12.4"
  override def mainClass = Some ("org.drsimonmiles.rocks.Create")
  val localLibs: Loose.Agg[PathRef] = Agg.from (ammonite.ops.ls (millSourcePath / "lib").map (PathRef (_)))
  override def unmanagedClasspath = T { localLibs }
}
