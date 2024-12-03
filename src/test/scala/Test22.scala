import org.scalatest._
import assignments.Day22

class Test22 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day22.partOne() should be(88226)
    Day22.partTwo() should be(57305)
  }
