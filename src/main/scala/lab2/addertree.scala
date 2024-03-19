package Lab2

import chisel3._
import chisel3.util._

class AdderTree(width: Int, numInputs: Int) extends Module {
  require(numInputs > 1, "AdderTree must have more than one input")

  val io = IO(new Bundle {
    val inputs = Input(Vec(numInputs, UInt(width.W)))
    val output = Output(UInt((width+log2Ceil(numInputs)).W))
  })

    // +&表示保留进位
    io.output := io.inputs.reduce(_ +& _)
}
