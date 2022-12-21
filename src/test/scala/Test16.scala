import org.scalatest._
import assignments.Day16

class Test16 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day16.partOne() should be(2330)
    Day16.partTwo() should be(2675)
  }
