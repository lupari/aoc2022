import org.scalatest._
import assignments.Day10

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day10.partOne() should be(14320)
    println("Should display PCPBKAPJ:")
    println(Day10.partTwo())
  }
