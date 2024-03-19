// Define a package named Adder
package Adder

// Import necessary libraries
import chisel3._
import chisel3.util._

/** A module that performs addition of two 64-bit unsigned integers.
    *
    * @param in_a The first input operand.
    * @param in_b The second input operand.
    * @param out_c The output sum of the two input operands.
*/

//define Uint64_Adder class by extend Module from chisel3 library\
class Uint64_Adder extends Module {        

    // define IO object as bundle (a collection of hardware elements)
    val io = IO(new Bundle {           // val is the way to define varaible in scala  
        val in_a = Input(UInt(64.W))   // input, data type is UInt, width is 64
        val in_b = Input(UInt(64.W))
        val out_c = Output(UInt(64.W))
    })

    // call attributes of io object to perform addition
    io.out_c := io.in_a + io.in_b   // := is the way to assign value in scala
}

class Uint32_Adder extends Module {
    val io = IO(new Bundle {
        val in_a = Input(UInt(32.W))
        val in_b = Input(UInt(32.W))
        val out_c = Output(UInt(32.W))
    })
    io.out_c := io.in_a + io.in_b
}