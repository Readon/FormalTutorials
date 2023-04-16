package counter

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

object GlobalClock {
  val gclk = ClockDomain.internal("_global").withBootReset()
  gclk.clock.addAttribute("gclk")

  def constraintClockDomain(domain: ClockDomain, period: Int, aligned: Boolean = false) = new ClockingArea(gclk) {
    val timer = CounterFreeRun(period)
    val phase = if (!aligned) timer.value + anyconst(cloneOf(timer.value)) else timer.value
    assume(domain.readClockWire === phase(timer.getBitsWidth - 1))
    val activeEdge = if (domain.config.clockEdge == RISING) rose(domain.readClockWire) else fell(domain.readClockWire)
    when(pastValid & !activeEdge) { assume(!fell(domain.isResetActive)) }
  }
  }
}

class FormalFifoCCTester extends SpinalFormalFunSuite {
  test("fifo-verify all") {
    val initialCycles = 2
    val inOutDelay = 2
    val coverCycles = 50
    val fifoDepth = 4
    val inClkPeriod = 5
    val outClkPeriod = 3
    FormalConfig
      .withProve(50)
      .withCover(coverCycles)
      .withAsync
      .withDebug
      .doVerify(new Component {
        val pushClock = ClockDomain.current
        val reset = ClockDomain.current.isResetActive
        val popClock = ClockDomain.external("pop")
        val popReset = popClock.isResetActive

        val inValue = in(UInt(3 bits))
        val inValid = in(Bool())
        val outReady = in(Bool())

        GlobalClock.constraintClockDomain(pushClock, inClkPeriod)
        GlobalClock.constraintClockDomain(popClock, outClkPeriod)

        val globalArea = new ClockingArea(GlobalClock.gclk) {
          val resetCounter = Counter(outClkPeriod)
          when(rose(reset) || (reset & resetCounter.value > 0)) { resetCounter.increment() }
          when(resetCounter.willOverflow) { resetCounter.clear() }
          when(resetCounter.value > 0) { assume(reset === True) }

          when(!rose(pushClock.readClockWire)) { assume(!changed(inValid)); assume(!changed(inValue)) }
          when(!rose(popClock.readClockWire)) { assume(!changed(outReady)) }
        }

        val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock))

        val checkArea = new ClockingArea(GlobalClock.gclk) {
          assert(dut.pushCC.pushPtrGray === toGray(dut.pushCC.pushPtr))
          assert(dut.popCC.popPtrGray === toGray(dut.popCC.popPtr))
          when(dut.io.push.ready) { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth - 1) }
            .otherwise { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth) }
          assert(dut.pushCC.pushPtr - fromGray(dut.pushCC.popPtrGray) <= fifoDepth)
          assert(fromGray(dut.popCC.pushPtrGray) - dut.popCC.popPtr <= fifoDepth)
        }

        assumeInitial(reset)
        assumeInitial(popReset)

        dut.io.push.payload := inValue
        dut.io.push.valid := inValid
        dut.io.pop.ready := outReady

        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.withAssumes()
        dut.io.push.withCovers()
        when(!reset & changed(dut.pushCC.popPtrGray)) {
          assert(fromGray(dut.pushCC.popPtrGray) - past(fromGray(dut.pushCC.popPtrGray)) <= fifoDepth - 1)
        }

        // back to back transaction cover test.
        val popArea = new ClockingArea(popClock) {
          dut.io.pop.withCovers(initialCycles)
          when(!reset) { dut.io.pop.withAsserts() }
          when(!reset & changed(dut.popCC.pushPtrGray)) {
            assert(fromGray(dut.popCC.pushPtrGray) - past(fromGray(dut.popCC.pushPtrGray)) <= fifoDepth - 1)
          }
        }
      })
  }
}
