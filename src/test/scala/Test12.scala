import org.scalatest._
import assignments.Day12

class Test12 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day12.partOne() should be(490)
    Day12.partTwo() should be(488)
  }
