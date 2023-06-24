package counter

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

case class GlobalClock() {
  val domain = ClockDomain.internal("_global").withBootReset()
  domain.clock.addAttribute("gclk")

  def constraintClockDomain(domain: ClockDomain, period: Int, aligned: Boolean = false) = new ClockingArea(domain) {
    val timer = CounterFreeRun(period)
    val phase = if (!aligned) timer.value + anyconst(cloneOf(timer.value)) else timer.value
    assume(domain.readClockWire === phase(timer.getBitsWidth - 1))
    val activeEdge = if (domain.config.clockEdge == RISING) rose(domain.readClockWire) else fell(domain.readClockWire)
    when(pastValid & !activeEdge) { assume(!fell(domain.isResetActive)) }
  }

  def clockAlignIO(domain: ClockDomain, signal: Data) = new ClockingArea(domain) {
    val activeEdge = if (domain.config.clockEdge == RISING) rose(domain.readClockWire) else fell(domain.readClockWire)
    when(pastValid & !activeEdge) { assume(!changed(signal)) }
  }

  def keepBoolLeastCycles(target: Bool, period: Int) = new ClockingArea(domain) {
    val timer = Timeout(period)
    when(!target & timer.counter.value === 0) { timer.clear() }
    when(timer.counter.value > 0) { assume(target === True) }
  }
}

class FormalFifoCCTester extends SpinalFormalFunSuite {
  def testMain(pushPeriod: Int, popPeriod: Int) = {
    val back2backCycles = 2

    val fifoDepth = 4
    val proveCycles = 8
    val coverCycles = 10
    val maxPeriod = Math.max(pushPeriod, popPeriod)

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
        val gclk = GlobalClock()

        gclk.constraintClockDomain(pushClock, pushPeriod)
        gclk.constraintClockDomain(popClock, popPeriod)
        gclk.keepBoolLeastCycles(reset, popPeriod)

        val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock))
        gclk.clockAlignIO(pushClock, dut.io.push.valid)
        gclk.clockAlignIO(pushClock, dut.io.push.payload)
        gclk.clockAlignIO(popClock, dut.io.pop.ready)

        val checkArea = new ClockingArea(gclk.domain) {
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

        dut.io.push.formalAssumesSlave()
        dut.io.push.formalCovers()
        when(!reset & changed(dut.pushCC.popPtrGray)) {
          assert(fromGray(dut.pushCC.popPtrGray) - past(fromGray(dut.pushCC.popPtrGray)) <= fifoDepth)
        }
        assert(dut.pushCC.pushPtrGray === toGray(dut.pushCC.pushPtr))
        assert(dut.pushCC.pushPtr - fromGray(dut.pushCC.popPtrGray) <= fifoDepth)

        // back to back transaction cover test.
        val popArea = new ClockingArea(popClock) {
          dut.io.pop.formalCovers(back2backCycles)
          when(!reset) { dut.io.pop.formalAssertsMaster() }
          when(!reset & changed(dut.popCC.pushPtrGray)) {
            assert(fromGray(dut.popCC.pushPtrGray) - past(fromGray(dut.popCC.pushPtrGray)) <= fifoDepth)
          }
        }
      })
  }

  test("fifo-verify fast pop") {
    testMain(5, 3)
  }

  test("fifo-verify fast push") {
    testMain(3, 5)
  }

  test("fifo-verify ultra fast pop") {
    testMain(11, 2)
  }

  test("fifo-verify ultra fast push") {
    testMain(2, 11)
  }
}
