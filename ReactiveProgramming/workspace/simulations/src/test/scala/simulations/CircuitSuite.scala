package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  val DemuxGateDelay = 10
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("demuxGate example") {
    val in, c, out0, out1 = new Wire
    demux(in, c::Nil, out1::out0::Nil)
    
    in.setSignal(true)
    c.setSignal(false)
    run
    assert(out0.getSignal === true, "dmux 1")
    assert(out1.getSignal === false, "dmux 2")
    
    c.setSignal(true)
    run
    assert(out0.getSignal === false, "dmux 3")
    assert(out1.getSignal === true, "dmux 4")
    
    in.setSignal(false)
    run
    assert(out0.getSignal === false, "dmux 5")
    assert(out1.getSignal === false, "dmux 6")

    c.setSignal(false)
    run
    assert(out0.getSignal === false, "dmux 7")
    assert(out1.getSignal === false, "dmux 8")
  }
  
//  test("demuxGate advanced example") {
//    val in = new Wire
//    val c = (new Wire)::(new Wire)::Nil
//    val out = (new Wire)::(new Wire)::(new Wire)::(new Wire)::Nil
//    demux(in, c.reverse, out.reverse)
//    
//    in.setSignal(false)
//    c(0).setSignal(true)
//    run
//    assert(!out.exists(x => x.getSignal), "dmux 0")
//    
//    c(1).setSignal(true)
//    run
//    assert(!out.exists(x => x.getSignal), "dmux 1")
//    
//    in.setSignal(true)
//    run
//    assert(out(3).getSignal === false, "dmux 2")
//    assert(out(0).getSignal === false, "dmux 3")
//    assert(out(1).getSignal === false, "dmux 4")
//    assert(out(2).getSignal === false, "dmux 5")
//    
//    c(0).setSignal(false)
//    run
//    assert(out(2).getSignal === false, "dmux 6")
//  }
}
