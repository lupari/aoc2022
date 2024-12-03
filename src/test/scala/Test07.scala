import org.scalatest._
import assignments.Day07

class Test07 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day07.partOne() should be(2031851)
    Day07.partTwo() should be(2568781)
  }
