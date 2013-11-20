package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notIn1, notIn2, notOut = new Wire
    inverter(a1, notIn1)
    inverter(a2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }
  
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def controlAction() {
      val inSig = in.getSignal
      def rec(controls : List[Wire], wires : List[Wire], size : Int): Unit = controls match {
    	case Nil => wires.head.setSignal(inSig)
    	case c::rest => {
    	  if (c.getSignal) {
    	    wires.take(size / 2).map(w => w.setSignal(false))
    		rec(rest, wires.drop(size / 2), size / 2)
    	  }
    	  else {
    	    wires.drop(size / 2).map(w => w.setSignal(false))
    	    rec(rest, wires.take(size / 2), size / 2)
    	  }
    	}
      }
      afterDelay(OrGateDelay) {
    	  rec(c, out.reverse, out.size)
      }
    }
    in addAction controlAction
    c.foreach(x => x addAction controlAction)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  val DemuxGateDelay = 7

  def andGateExample {
    println("---andGateExample---")
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
    
  }
  
  def orGateExample {
    println("---orGateExample---")
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(true)
    in2.setSignal(false)
    run
    
    in1.setSignal(false)
    run
    
    in2.setSignal(true)
    run
  }

  def demuxGateExample {
    println("---demuxExample---")
    val in, c, out0, out1 = new Wire
    demux(in, c::Nil, out0::out1::Nil)
    probe("in", in)
    probe("c", c)
    probe("out0", out0)
    probe("out1", out1)
    
    in.setSignal(true)
    c.setSignal(false)
    run
    
    in.setSignal(true)
    c.setSignal(true)
    run
    
    in.setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.demuxGateExample
}
