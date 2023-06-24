package counter

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalFifoCCTester extends SpinalFormalFunSuite {
  test("fifo-verify all") {
    val initialCycles = 2
    val inOutDelay = 2
    val coverCycles = 50
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
        
        val inValue = in (UInt(3 bits))
        val inValid = in (Bool())
        val outReady = in (Bool())
        val globalClock = ClockDomain.internal("global").withBootReset()
        globalClock.clock.addAttribute("gclk")

        val globalArea = new ClockingArea(globalClock) {
          val timer = CounterFreeRun(5)
          val phase = anyconst(cloneOf(timer.value))
          assume(pushClock.readClockWire === (timer.value + phase)(timer.getBitsWidth - 1))
          when(pastValid & !rose(pushClock.readClockWire)) { assume(!fell(pushClock.isResetActive)) }

          val popTimer = CounterFreeRun(2)
          val popPhase = anyconst(cloneOf(popTimer.value))
          assume(popClock.readClockWire === (popTimer.value + popPhase)(popTimer.getBitsWidth - 1))

          when(!rose(pushClock.readClockWire)) { assume(!changed(inValid)); assume(!changed(inValue))}
          val resetCounter = Counter(15)
          when(rose(reset) || (reset & resetCounter.value > 0)) { resetCounter.increment() }
          when(resetCounter.willOverflow) { resetCounter.clear() }
          when(resetCounter.value > 0) { assume(reset === True) }

          when(!rose(popClock.readClockWire)) { assume(!fell(popClock.isResetActive)) }
          when(!rose(popClock.readClockWire)) { assume(!changed(outReady))}
        }

        val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), 4, pushClock, popClock, false))

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
        // dut.withAssumes()
        
        dut.io.push.formalCovers()
        // back to back transaction cover test.
        val popArea = new ClockingArea(popClock) {
          dut.io.pop.formalCovers(initialCycles)
          // when(!globalArea.resetToPop) { dut.io.pop.formalAssertsMaster() }
          when(!reset) { dut.io.pop.formalAssertsMaster() }
        }
      })
  }
}