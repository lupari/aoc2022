import org.scalatest._
import assignments.Day20

class Test20 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day20.partOne() should be(1087)
    Day20.partTwo() should be(13084440324666L)
  }
