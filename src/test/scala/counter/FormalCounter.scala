package counter

import spinal.core.formal._
import spinal.core._
import spinal.lib._

class LimitedCounter extends Component {
  // The value register will always be between [2:10]
  val value = Reg(UInt(4 bits)) init (2)
  when(value < 10) {
    value := value + 1
  }
}

class FormalCounterTester extends SpinalFormalFunSuite {
  def tester() {
    FormalConfig
      .withBMC(10)
      // .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new LimitedCounter())
        val reset = ClockDomain.current.isResetActive
      })
  }
  test("formal_tester") {
    tester()
  }
}
