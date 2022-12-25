import org.scalatest._
import assignments.Day24

class Test24 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day24.partOne() should be(286)
    Day24.partTwo() should be(820)
  }
