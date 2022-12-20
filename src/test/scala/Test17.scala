import org.scalatest._
import assignments.Day17

class Test17 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day17.partOne() should be(3200)
    Day17.partTwo() should be(1584927536247L)
  }
