package counter

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

case class GlobalClock() {
  val domain = ClockDomain.internal("_global").withBootReset()
  domain.clock.addAttribute("gclk")

  def assumeClockTiming(target: ClockDomain, period: Int, aligned: Boolean = false) = new ClockingArea(domain) {
    val timer = CounterFreeRun(period)
    val phase = if (!aligned) timer.value + anyconst(cloneOf(timer.value)) else timer.value
    assume(target.readClockWire === phase(timer.getBitsWidth - 1))
  }

  def assumeResetReleaseSync(target: ClockDomain) = new ClockingArea(domain) {
    if (target.hasResetSignal) {
      val activeEdge = if (target.config.clockEdge == RISING) rose(target.readClockWire) else fell(target.readClockWire)
      if (target.config.resetKind == SYNC) {
        when(pastValid & !activeEdge) { assume(!changed(target.isResetActive)) }
      } else {
        when(pastValid & !activeEdge) { assume(!fell(target.isResetActive)) }
      }
    }
  }

  def assumeIOSync2Clock(target: ClockDomain, signal: Data) = new ClockingArea(domain) {
    val activeEdge = if (target.config.clockEdge == RISING) rose(target.readClockWire) else fell(target.readClockWire)
    when(pastValid & !activeEdge) { assume(!changed(signal)) }
  }

  def keepBoolLeastCycles(target: Bool, period: Int) = new ClockingArea(domain) {
    val timer = Timeout(period)
    when(!target & timer.counter.value === 0) { timer.clear() }
    when(timer.counter.value > 0) { assume(target === True) }
  }

  def alignAsyncResetStart(src: ClockDomain, dst: ClockDomain) = new ClockingArea(domain) {
    if (src.hasResetSignal && dst.hasResetSignal && src.config.resetKind == ASYNC && dst.config.resetKind == ASYNC) {
      when(pastValid) { assume(rose(src.isResetActive) === rose(dst.isResetActive)) }
      when(pastValid & !src.isResetActive & rose(dst.readClockWire)) { assume(dst.isResetActive === False) }
      when(pastValid & !dst.isResetActive & rose(src.readClockWire)) { assume(src.isResetActive === False) }
    }
  }
}

class FormalFifoCCTester extends SpinalFormalFunSuite {
  def formalContext(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = new Area {
    val back2backCycles = 2
    val fifoDepth = 4

    val pushClock = ClockDomain.current
    val reset = pushClock.isResetActive
    val popClock = ClockDomain.external("pop")
    val popReset = popClock.isResetActive

    val inValue = in(UInt(3 bits))
    val inValid = in(Bool())
    val outReady = in(Bool())
    val gclk = GlobalClock()

    assumeInitial(reset)
    assumeInitial(popReset)

    gclk.assumeClockTiming(pushClock, pushPeriod)
    gclk.assumeResetReleaseSync(pushClock)
    if (!seperateReset) {
      gclk.keepBoolLeastCycles(reset, popPeriod)
    }

    gclk.assumeClockTiming(popClock, popPeriod)
    if (seperateReset) {
      gclk.assumeResetReleaseSync(popClock)
      gclk.alignAsyncResetStart(pushClock, popClock)
    }

    val dut = FormalDut(new StreamFifoCC(cloneOf(inValue), fifoDepth, pushClock, popClock, !seperateReset))
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.valid)
    gclk.assumeIOSync2Clock(pushClock, dut.io.push.payload)
    gclk.assumeIOSync2Clock(popClock, dut.io.pop.ready)

    dut.io.push.payload := inValue
    dut.io.push.valid := inValid
    dut.io.pop.ready := outReady

    // assume no valid while reset and one clock later.
    when(reset || past(reset)) {
      assume(inValid === False)
    }

    val globalArea = new ClockingArea(gclk.domain) {
      when(dut.io.push.ready) { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth - 1) }
        .otherwise { assert(dut.pushCC.pushPtr - dut.popCC.popPtr <= fifoDepth) }

      assert(dut.popCC.popPtrGray === toGray(dut.popCC.popPtr))
      assert(fromGray(dut.popCC.pushPtrGray) - dut.popCC.popPtr <= fifoDepth)
    }

    val pushArea = new ClockingArea(pushClock) {
      dut.io.push.withAssumes()
      dut.io.push.withCovers()

      when(pastValid & changed(dut.pushCC.popPtrGray)) {
        assert(fromGray(dut.pushCC.popPtrGray) - past(fromGray(dut.pushCC.popPtrGray)) <= fifoDepth)
      }
      assert(dut.pushCC.pushPtrGray === toGray(dut.pushCC.pushPtr))
      assert(dut.pushCC.pushPtr - fromGray(dut.pushCC.popPtrGray) <= fifoDepth)
    }

    // back to back transaction cover test.
    val popCheckDomain = if (seperateReset) popClock else popClock.copy(reset = reset)
    val popArea = new ClockingArea(popCheckDomain) {
      dut.io.pop.withCovers(back2backCycles)
      dut.io.pop.withAsserts()

      when(pastValid & changed(dut.popCC.pushPtrGray)) {
        assert(fromGray(dut.popCC.pushPtrGray) - past(fromGray(dut.popCC.pushPtrGray)) <= fifoDepth)
      }
    }
  }

  def testMain(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = {
    val proveCycles = 8
    val coverCycles = 8
    val maxPeriod = Math.max(pushPeriod, popPeriod)

    FormalConfig
      .withProve(maxPeriod * proveCycles)
      .withCover(maxPeriod * coverCycles)
      .withAsync
      .withDebug
      .doVerify(new Component {
        val context = formalContext(pushPeriod, popPeriod, seperateReset)
      })
  }

  test("fifo-verify fast pop") {
    testMain(5, 3)
  }

  test("fifo-verify fast push") {
    testMain(3, 5)
  }

  // test("fifo-verify ultra fast pop") {
  //   testMain(11, 2)
  // }

  // test("fifo-verify ultra fast push") {
  //   testMain(2, 11)
  // }

  test("fifo-verify fast pop reset seperately") {
    testMain(5, 3, true)
  }

  test("fifo-verify fast push reset seperately") {
    testMain(3, 5, true)
  }

  // test("fifo-verify ultra fast pop reset seperately") {
  //   testMain(11, 2, true)
  // }

  // test("fifo-verify ultra fast push reset seperately") {
  //   testMain(2, 11, true)
  // }

  def testNoLoss(pushPeriod: Int, popPeriod: Int, seperateReset: Boolean = false) = {
    val proveCycles = 8
    val coverCycles = 8
    val maxPeriod = Math.max(pushPeriod, popPeriod)

    FormalConfig
      .withCover(maxPeriod * coverCycles)
      .withAsync
      .withDebug
      .doVerify(new Component {
        val context = formalContext(pushPeriod, popPeriod, seperateReset)
        
        val d1 = anyconst(cloneOf(context.inValue))
        val d1_in = RegInit(False)
        when(context.inValue === d1 & context.dut.io.push.fire) {
          d1_in := True
        }
        when(d1_in) { assume(context.inValue =/= d1) }

        val popCheckArea = new ClockingArea(context.popCheckDomain) {
          val d1_inCC = BufferCC(d1_in)
          val cond = d1_inCC & context.dut.io.pop.fire
          val hist = History(context.dut.io.pop.payload, context.fifoDepth, cond)
          val counter = Counter(context.fifoDepth, inc = cond)
          when(!d1_inCC) { counter.clear() }
          cover(counter.willOverflow & !hist.sContains(d1))
        }

        val globalCheckArea = new ClockingArea(context.popCheckDomain) {
          def checkValidData(cond: UInt => Bool): Seq[Bool] = context.dut.rework {
            val maps = for (i <- 0 until context.fifoDepth) yield {
              val index = context.dut.popCC.popPtr + i
              val out = False
              when(i < context.dut.pushCC.pushPtr - context.dut.popCC.popPtr) {
                out := cond(context.dut.ram(index.resized))
              }
              out
            }
            maps
          }
          val fits = checkValidData(_ === d1.pull())
          val containD1 = fits.reduce(_ || _)
          when(!d1_in) { assume(!containD1) }
        }
      })
  }

  test("noloss fast pop") {
    shouldFail(testNoLoss(5, 3))
  }

  test("noloss fast push") {
    shouldFail(testNoLoss(3, 5))
  }

  // test("noloss ultra fast pop") {
  //   shouldFail(testNoLoss(11, 2))
  // }

  // test("noloss ultra fast push") {
  //   shouldFail(testNoLoss(2, 11))
  // }

  test("noloss fast pop reset seperately") {
    shouldFail(testNoLoss(5, 3, true))
  }

  test("noloss fast push reset seperately") {
    shouldFail(testNoLoss(3, 5, true))
  }

  // test("noloss ultra fast pop reset seperately") {
  //   shouldFail(testNoLoss(11, 2, true))
  // }

  // test("noloss ultra fast push reset seperately") {
  //   shouldFail(testNoLoss(2, 11, true))
  // }
}
