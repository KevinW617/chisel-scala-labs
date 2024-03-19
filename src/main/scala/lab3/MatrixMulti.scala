package Lab3

import chisel3._
import chisel3.util._

case class MatMulConfig(
                        vecDWidth: Int = 8,     // A B的精度8b
                        resDWidth: Int = 32,    // 结果的精度
                        SEL_A: UInt=0.U,        // 针对A的操作
                        matSize: Int = 16,       // 矩阵的大小 16*16
                        SEL_B: UInt=1.U,
                        SEL_C: UInt=2.U,
                        OP_ACCESS_MEM: UInt=1.U,    // Top模块的操作选择-访问memory
                        OP_COMPUTE: UInt=0.U,       // Top模块的操作选择-执行计算
                        )
{
    val mat_elem_num= matSize*matSize
    val memory_size= mat_elem_num*3    // A B C
    val matA_baseAddr=0
    val matB_baseAddr=mat_elem_num
    val matC_baseAddr=2*mat_elem_num
    val memory_width=resDWidth
}

class MM_TOP(cfg:MatMulConfig) extends Module
{
    val io = IO(new Bundle
    {
        // ******** Load Data to Memory **********
        val dataIn=Input(SInt(cfg.memory_width.W))
        val dataOut=Output(SInt(cfg.memory_width.W))
        val addr=Input(UInt(log2Ceil(cfg.memory_size).W))
        val writeEn=Input(Bool())
        val enable=Input(Bool())

        // operation: ld/st or exe
        val op=Input(UInt(2.W))

        // start mm module
        val start=Input(Bool())

        // compute finish
        val busy=Output(Bool())
    })

    val memory_module=Module(new MatMem(cfg.memory_size,cfg.memory_width))

    val mat_module=Module(new MatMulModule(cfg))

    // ---------Connection---------
    val mat_mul_access_mem= (io.op=/=cfg.OP_COMPUTE)
    memory_module.io.addr:=Mux(mat_mul_access_mem,io.addr,mat_module.io.addr)
    memory_module.io.writeEn:=Mux(mat_mul_access_mem,io.writeEn,mat_module.io.writeEn)
    memory_module.io.en:=Mux(mat_mul_access_mem,io.enable,mat_module.io.enable)
    memory_module.io.dataIn:=Mux(mat_mul_access_mem,io.dataIn,mat_module.io.dataOut)
    mat_module.io.dataIn:=memory_module.io.dataOut
    mat_module.io.start:=io.start
    io.dataOut:=memory_module.io.dataOut.asSInt
    io.busy:=mat_module.io.busy

}

class MatMem(MEM_ROW:Int,MEM_WIDTH:Int) extends Module 
{
    val io = IO(new Bundle {
        val addr = Input(UInt(log2Ceil(MEM_ROW).W))
        val writeEn = Input(Bool())
        val en = Input(Bool())
        val dataIn = Input(SInt(MEM_WIDTH.W))
        val dataOut = Output(SInt(MEM_WIDTH.W))
    })

    val mem = SyncReadMem(MEM_ROW,SInt(MEM_WIDTH.W))

    // single port 
    when(io.writeEn & io.en){
        mem.write(io.addr,io.dataIn)
    }
    io.dataOut := mem.read(io.addr, io.en&&(!io.writeEn))
}

class MatMulModule(cfg:MatMulConfig) extends Module 
{
     val io = IO(new Bundle {
        // 访问memeory的IOs
        val addr = Output(UInt(log2Ceil(cfg.memory_size).W))
        val writeEn = Output(Bool())
        val enable = Output(Bool())
        val dataIn = Input(SInt(cfg.memory_width.W))
        val dataOut = Output(SInt(cfg.memory_width.W))

        // 开始执行信号
        val start=Input(Bool())

        // 是否执行完成信号 busy是true.B表示正在执行
        val busy=Output(Bool())
    })
    // Mat A
    val matA_buf=RegInit(VecInit(Seq.fill(cfg.mat_elem_num)(0.S(cfg.resDWidth.W))))
    // Mat B
    val matB_buf=RegInit(VecInit(Seq.fill(cfg.mat_elem_num)(0.S(cfg.resDWidth.W))))
    // Mat C
    val matC_buf=RegInit(VecInit(Seq.fill(cfg.mat_elem_num)(0.S(cfg.resDWidth.W))))
    // 默认赋值
    io.dataOut := 0.S
    io.addr := 0.U
    io.writeEn := false.B
    io.enable := true.B
    io.busy := true.B

    val state=RegInit(0.U(3.W))
    val index=RegInit(0.U(log2Ceil(cfg.mat_elem_num+1).W))
    val buf_sel=RegInit(cfg.SEL_A)
    val row=RegInit(0.U(log2Ceil(cfg.matSize).W))
    val col=RegInit(0.U(log2Ceil(cfg.matSize).W))

    val en_load = WireInit(false.B)
    val en_store = WireInit(false.B)

    switch(state) {
      is(0.U){
        io.busy := true.B
        when(io.start){
          state := 1.U
          index := 0.U
        }
      }
      is(1.U){
        io.enable := true.B
        io.writeEn := false.B
        //load matA to buffer
        when(index <= cfg.mat_elem_num.U && buf_sel === cfg.SEL_A){
          io.addr := cfg.matA_baseAddr.U + index
          when(index =/= 0.U){
            matA_buf(index-1.U) := io.dataIn
          }
          index := index + 1.U
          state := 1.U
        }.otherwise{
          buf_sel := cfg.SEL_B
          index := 0.U
          state := 2.U
        }
      }
      is(2.U){
        io.enable := true.B
        io.writeEn := false.B
        //load matB to buffer
        when(index <= cfg.mat_elem_num.U && buf_sel === cfg.SEL_B){
          io.addr := cfg.matB_baseAddr.U + index
          when(index =/= 0.U){
            matB_buf(index-1.U) := io.dataIn.asSInt
          }
          index := index + 1.U
          state := 2.U
        }.otherwise{
          index := 0.U
          buf_sel := cfg.SEL_C
          state := 3.U
        }
      }
      is(3.U){
        io.enable := true.B
        io.writeEn := true.B
        val dotProduct = (0 until cfg.matSize).map(k => matA_buf(row*cfg.matSize.U+k.U) * matB_buf(k.U*cfg.matSize.U+col).asSInt).reduce(_ + _)
        io.addr := cfg.matC_baseAddr.U + row * cfg.matSize.U + col
        io.dataOut := dotProduct
        when(row === (cfg.matSize - 1).U && col === (cfg.matSize - 1).U) {
          row := 0.U
          col := 0.U
          state := 4.U
        }.otherwise {
          when(col === (cfg.matSize - 1).U) {
            row := row + 1.U
            col := 0.U
          }.otherwise {
            col := col + 1.U
          }
          state := 3.U
        }
      }
      is(4.U){
        io.busy := false.B
        state := 0.U
        buf_sel := cfg.SEL_A
      }
    }
 }