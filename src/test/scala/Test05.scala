import org.scalatest._
import assignments.Day05

class Test05 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day05.partOne() should be("TPGVQPFDH")
    Day05.partTwo() should be("DMRDFRHHH")
  }
