package Lab2

import chisel3._
import chisel3.util._

case class vecDotFSM_Configs(
                        vecDWidth: Int = 8,
                        resDWidth: Int = 32,
                        vecLen: Int = 16,
                        vecDepth: Int = 4,
                        SEL_A: Int=0,
                        SEL_B: Int=1,
                        SEL_D: Int=2
                        )

class FSM_VecDotProductModule(cfg:vecDotFSM_Configs) extends Module
{
    val io = IO(new Bundle
    {
        // ******** Input **********
        val dataIn = Input(UInt(cfg.vecDWidth.W))
        val ld_en = Input(Bool())   // 加载
        val buf_sel =Input(UInt(log2Ceil(3).W))   // 加载哪个向量 A or B or d , 3个选项，宽度为log2Ceil(3) 向上取整
        val dept_ptr =Input(UInt(log2Ceil(cfg.vecDepth).W))    // 加载由buf_sel选定的buf(A/B)中的哪个向量; 该信号同时也用于指定加载d中的哪个元素
        val elem_ptr =Input(UInt(log2Ceil(cfg.vecLen).W))     // 加载由buf_sel选定的buf(A/B)中，并由dept_ptr确定的向量中的哪个元素; 
        val start = Input(Bool())       // 加载完成后，给信号开始计算(启动状态机)
        
        // ******** Output **********
        val dataOut =Output(UInt(cfg.resDWidth.W))
        val finish=Output(Bool())  // 状态机计算完后给出结束信号表示算完了 
    })
     // Implementation todo
     //寄存器组定义
    val vec_A_groups = Reg(Vec(cfg.vecDepth, Vec(cfg.vecLen, UInt(cfg.vecDWidth.W))))
    val vec_B_groups = Reg(Vec(cfg.vecDepth, Vec(cfg.vecLen, UInt(cfg.vecDWidth.W))))
    val c_buffer = Reg(Vec(cfg.vecDepth, UInt(cfg.resDWidth.W)))
    // 加载  注意这里的循环并非执行顺序，只是开了这些硬件窗口，只有等到某个元素对应的信号到来时，才会Load，所以一定要加上io.dept_ptr===depth.U
      for(depth <- 0 until cfg.vecDepth){
        for(elem <- 0 until cfg.vecLen)
            {
                vec_A_groups(depth)(elem):=Mux(io.ld_en && io.buf_sel===cfg.SEL_A.U && io.elem_ptr===elem.U && io.dept_ptr === depth.U,io.dataIn,vec_A_groups(depth)(elem))
            }
        for(elem <- 0 until cfg.vecLen)
            {
                vec_B_groups(depth)(elem):=Mux(io.ld_en && io.buf_sel===cfg.SEL_B.U && io.elem_ptr===elem.U && io.dept_ptr === depth.U,io.dataIn,vec_B_groups(depth)(elem))
            }
        c_buffer(depth):=Mux(io.ld_en && io.buf_sel===cfg.SEL_D.U && io.dept_ptr===depth.U,io.dataIn,c_buffer(depth))
      }

    //状态机定义
    val depthReg = RegInit(0.U(log2Ceil(cfg.vecDepth).W))
    val state = RegInit(0.U(2.W))

    val temp = RegInit(0.U(cfg.resDWidth.W))

    io.finish := false.B
    io.dataOut := 0.U
    //逻辑实现
    when(io.start) {
    // 状态机开始
    state := 1.U
  }

  switch(state) {
    is(0.U) {
      // 等待开始信号
      when(io.start) {
        //io.dataOut := 0.U
        state := 1.U
      }
    }
    is(1.U) {
    // 乘
      val productRes = WireInit(VecInit(Seq.fill(cfg.vecLen)(0.U(cfg.resDWidth.W))))
      for (index <- 0 until cfg.vecLen) {
        productRes(index) := (vec_A_groups(depthReg)(index)) * (vec_B_groups(depthReg)(index))
      }
    // 加
      val adderTree=Module(new AdderTree(width=cfg.resDWidth,numInputs=cfg.vecLen))
      adderTree.io.inputs.zip(productRes).foreach { case (input, product) =>
      input := product
    }
    // 加上偏置，将当前所要计算的深度的结果保存到结果寄存器
      printf(p"depthReg = $depthReg, c_buffer = ${c_buffer(depthReg)}\n")
      printf(p"adderTree.io.output = ${adderTree.io.output}\n")
    printf(p"c_buffer(depthReg) = ${c_buffer(depthReg)}\n")
    temp := adderTree.io.output + c_buffer(depthReg)
    printf(p"io.dataOut = ${io.dataOut}\n")
    // 跳转到下一个状态
    state := 2.U
    }
    is(2.U) {
       // 返回一个计算完成信号
      io.dataOut := temp
      io.finish := true.B
      // 检查是否是最后一个深度
      when(depthReg === (cfg.vecDepth - 1).U) {
        // 如果是最后一个深度，回到状态0
        depthReg := 0.U
        state := 0.U
      } otherwise {
        // 如果不是最后一个深度，继续计算下一个深度
        depthReg := depthReg + 1.U
        state := 1.U
      }
    }
}
}