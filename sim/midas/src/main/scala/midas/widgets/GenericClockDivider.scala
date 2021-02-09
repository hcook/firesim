// See LICENSE for license details.

package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.experimental.BaseModule

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.unittest.{UnitTest}

/**
  * This models a generic integer divider, whose first output positive edge is
  * coincident with the first input positive edge.
  *
  * Note: The output clock does not have a 50% duty cycle for odd divisions.
  */
class GenericClockDividerN(div: Int) extends MultiIOModule {
  val clk_in   = IO(Flipped(new TimestampedTuple(Bool())))
  val clk_out  = IO(new TimestampedTuple(Bool()))
  if (div == 1) {
    clk_out <> clk_in
  } else {
    val lowTransition = div / 2
    val highTransition = div - lowTransition
    val edgeCount = RegInit((div - 1).U(log2Ceil(div).W))
    val last = Reg(Bool())
    val doneInit = RegInit(false.B)

    clk_out.latest.valid := clk_in.latest.valid
    clk_out.latest.bits.time := clk_in.latest.bits.time
    clk_out.latest.bits.data := edgeCount < lowTransition.U

    // We only potentially stall when there's a positive edge on the input.
    // Could do better and only potentially stall on output transitions...
    clk_in.observed := clk_out.observed || clk_in.unchanged || !clk_in.latest.bits.data
    val iPosedge = clk_in.latest.valid && !clk_in.unchanged && clk_in.latest.bits.data

    // Time Zero
    when(!clk_out.old.valid && clk_out.observed) {
      edgeCount := Mux(clk_in.latest.bits.data, highTransition.U, 0.U)
      clk_out.latest.bits.data := clk_in.latest.bits.data
    // All subsequent input posedges
    }.elsewhen(iPosedge) {
      edgeCount := edgeCount + 1.U
      when(edgeCount === highTransition.U && clk_out.observed) {
        edgeCount := 0.U
        clk_out.latest.bits.data := true.B
      }.elsewhen(edgeCount === lowTransition.U && clk_out.observed) {
        clk_out.latest.bits.data := false.B
      }
    }
  }
}
