package Lab2

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class VecDotProductTest extends AnyFlatSpec with ChiselScalatestTester {

  "VecDotProduct test" should "pass" in {
    val test_cfg=ModuleConfigs()
    test(new VecDotProductModule(cfg=test_cfg)) { dut =>
        // 进行10组测试
        for(testID <- 0 until 10)
        {
            dut.clock.step(1)
            println(s"Begin test ${testID}")
            // data preparision & poke
            val vec_A=gen_randomVec(nElem=test_cfg.vecLen,elemWidth=test_cfg.vecDWidth)
            val vec_B=gen_randomVec(nElem=test_cfg.vecLen,elemWidth=test_cfg.vecDWidth)
            val true_dot_product_res=get_true_mac(vec_A=vec_A,vec_B=vec_B,vecLen=test_cfg.vecLen)
            println(s"true_dot_product_res=0x${true_dot_product_res.toString(16)}")

            
            // input A
            dut.io.ld_en.poke(true)
            dut.io.buf_sel.poke(test_cfg.SEL_A)
            var input_ptr=0
            for(in <- 0 until test_cfg.vecLen)
            {
              dut.io.dataIn.poke(vec_A(in).U)
              dut.io.dataIn_ptr.poke(input_ptr.U)
              dut.clock.step(1)
              input_ptr=input_ptr+1
            }

            // input B
            dut.io.ld_en.poke(true)
            dut.io.buf_sel.poke(test_cfg.SEL_B)
            input_ptr=0
            for(in <- 0 until test_cfg.vecLen)
            {
              dut.io.dataIn.poke(vec_B(in).U)
              dut.io.dataIn_ptr.poke(input_ptr.U)
              dut.clock.step(1)
              input_ptr=input_ptr+1
            }
            dut.io.ld_en.poke(false)

            // read output
            dut.io.dataOut.expect(true_dot_product_res.U)
            dut.clock.step(1)
        }
        
      }
    }

    def gen_randomVec(nElem:Int,elemWidth:Int): Array[BigInt] =
    {
      val dataArray=Array.ofDim[BigInt](nElem)

      for(i <- 0 until nElem)
      {
        dataArray(i)=BigInt(elemWidth, scala.util.Random)
      }

      return dataArray
    }

    def get_true_mac(vec_A: Array[BigInt],vec_B: Array[BigInt],vecLen:Int):BigInt=
    {
      var trueSum: BigInt = BigInt(0)
      for(elem <- 0 until vecLen)
      {
        trueSum=trueSum+vec_A(elem)*vec_B(elem)
      }
      return trueSum
    }
}
