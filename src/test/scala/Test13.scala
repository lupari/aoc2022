import org.scalatest._
import assignments.Day13

class Test13 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day13.partOne() should be(5003)
    Day13.partTwo() should be(20280)
  }
