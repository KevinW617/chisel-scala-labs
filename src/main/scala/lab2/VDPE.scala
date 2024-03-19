/**
 * This file contains the definition of the VecDotProductModule class, which implements a vector dot product module.
 * The module takes two input vectors, A and B, and calculates their dot product by multiplying corresponding elements
 * and summing the results. The module supports configurable vector and result data widths, vector length, and selection
 * between vectors A and B. It also provides an enable signal for loading data into the vectors.
 *
 * @param cfg The configuration parameters for the module.
 */
 
package Lab2

import chisel3._
import chisel3.util._

case class ModuleConfigs(
                        vecDWidth: Int = 8,  // vector data bit width
                        resDWidth: Int = 32, // result data bit width
                        vecLen: Int = 16, // vector length
                        SEL_A:Bool=true.B, // select vector A
                        SEL_B:Bool=false.B // select vector B
                        )

class VecDotProductModule(cfg:ModuleConfigs) extends Module  // take the ModuleConfigs as a parameter
{
    val io = IO(new Bundle
    {
        val dataIn = Input(UInt(cfg.vecDWidth.W)) // data input wire
        val ld_en = Input(Bool())  // load enable wire
        val buf_sel =Input(Bool())  // buffer select wire
        val dataIn_ptr =Input(UInt(log2Ceil(cfg.vecLen).W)) // data input pointer wire (address)
        val dataOut =Output(UInt(cfg.resDWidth.W))
    })

    // 向量A的寄存器组
    val vec_A = RegInit(VecInit(Seq.fill(cfg.vecLen)(0.U(cfg.vecDWidth.W))))

    // 向量B的寄存器组
    val vec_B = RegInit(VecInit(Seq.fill(cfg.vecLen)(0.U(cfg.vecDWidth.W))))

    // 加载向量A
    for(i <- 0 until cfg.vecLen)
    {   vec_A(i):=Mux(io.ld_en && io.buf_sel===cfg.SEL_A && io.dataIn_ptr===i.U,io.dataIn,vec_A(i))    }

    // 加载向量B
    for(i <- 0 until cfg.vecLen)
    {   vec_B(i):=Mux(io.ld_en && io.buf_sel===cfg.SEL_B && io.dataIn_ptr===i.U,io.dataIn,vec_B(i))    }

    // 乘
    val productRes=WireInit(VecInit(Seq.fill(cfg.vecLen)(0.U(cfg.resDWidth.W))))
    productRes.zipWithIndex.map{case(product_res,index)=>
        product_res:=vec_A(index)*vec_B(index)
    }

    // 加
    val adderTree=Module(new AdderTree(width=cfg.resDWidth,numInputs=cfg.vecLen))
    adderTree.io.inputs:=productRes

    // result
    io.dataOut:=adderTree.io.output
}