import org.scalatest._
import assignments.Day15

class Test15 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day15.partOne() should be(6124805)
    Day15.partTwo() should be(12555527364986L)
  }
