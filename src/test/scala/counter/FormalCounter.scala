package counter

import spinal.core.formal._
import spinal.core._
import spinal.lib._

class LimitedCounter extends Component {
  // The value register will always be between [2:10]
  val inc = in Bool()
  val value = Reg(UInt(4 bits)) init (2)
  when(inc && value < 10) {
    value := value + 1
  }
}

class FormalCounterTester extends SpinalFormalFunSuite {
  def tester() {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new LimitedCounter())
        val inc = in Bool()
        dut.inc <> inc
        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        for(i <- 2 to 9) {
          cover(dut.value === i)
        }

        when(pastValid && past(inc) && dut.value < 10){
          // assert(changed(dut.value))
          assert(dut.value === past(dut.value) + 1)
        }
        
        assert(dut.value <= 10 && dut.value >= 2)
      })
  }
  test("formal_tester") {
    tester()
  }
}
