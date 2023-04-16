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

  def clockAlignIO(domain: ClockDomain, signal: Data) = new ClockingArea(gclk) {
    val activeEdge = if (domain.config.clockEdge == RISING) rose(domain.readClockWire) else fell(domain.readClockWire)
    when(pastValid & !activeEdge) { assume(!changed(signal)) }
  }

  def keepBoolLeastCycles(target: Bool, period: Int) = new ClockingArea(gclk) {
    val timer = Timeout(period)
    when(!target & timer.counter.value === 0) { timer.clear() }
    when(timer.counter.value > 0) { assume(target === True) }
  }

  }
}

class FormalFifoCCTester extends SpinalFormalFunSuite {
  test("fifo-verify all") {
    val back2backCycles = 2

    val fifoDepth = 4
    val proveCycles = 8
    val coverCycles = 10

    val inClkPeriod = 5
    val outClkPeriod = 17
    val maxPeriod = Math.max(inClkPeriod, outClkPeriod)
    FormalConfig
      .withProve(maxPeriod * proveCycles)
      .withCover(maxPeriod * coverCycles)
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
        GlobalClock.keepBoolLeastCycles(reset, outClkPeriod)

        val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock))
        GlobalClock.clockAlignIO(pushClock, dut.io.push.valid)
        GlobalClock.clockAlignIO(pushClock, dut.io.push.payload)
        GlobalClock.clockAlignIO(popClock, dut.io.pop.ready)

        val checkArea = new ClockingArea(GlobalClock.gclk) {
          when(dut.io.push.ready) { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth - 1) }
            .otherwise { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth) }

          assert(dut.popCC.popPtrGray === toGray(dut.popCC.popPtr))
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
          assert(fromGray(dut.pushCC.popPtrGray) - past(fromGray(dut.pushCC.popPtrGray)) <= fifoDepth)
        }
        assert(dut.pushCC.pushPtrGray === toGray(dut.pushCC.pushPtr))
        assert(dut.pushCC.pushPtr - fromGray(dut.pushCC.popPtrGray) <= fifoDepth)

        // back to back transaction cover test.
        val popArea = new ClockingArea(popClock) {
          dut.io.pop.withCovers(back2backCycles)
          when(!reset) { dut.io.pop.withAsserts() }
          when(!reset & changed(dut.popCC.pushPtrGray)) {
            assert(fromGray(dut.popCC.pushPtrGray) - past(fromGray(dut.popCC.pushPtrGray)) <= fifoDepth)
          }
        }
      })
  }
}
