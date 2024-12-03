import org.scalatest._
import assignments.Day14

class Test14 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day14.partOne() should be(1068)
    Day14.partTwo() should be(27936)
  }
