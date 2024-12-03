import org.scalatest._
import assignments.Day23

class Test23 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day23.partOne() should be(3864)
    Day23.partTwo() should be(946)
  }
