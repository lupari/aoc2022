import org.scalatest._
import assignments.Day21

class Test21 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day21.partOne() should be(75147370123646d)
    Day21.partTwo() should be(3423279932937d)
  }
