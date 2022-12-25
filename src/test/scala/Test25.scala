import org.scalatest._
import assignments.Day25

class Test25 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day25.partOne() should be("2=20---01==222=0=0-2")
  }
