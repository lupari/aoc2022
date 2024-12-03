import org.scalatest._
import assignments.Day11

class Test11 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day11.partOne() should be(76728)
    Day11.partTwo() should be(21553910156L)
  }
