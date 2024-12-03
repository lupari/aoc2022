import org.scalatest._
import assignments.Day19

class Test19 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day19.partOne() should be(2193)
    Day19.partTwo() should be(7200)
  }
