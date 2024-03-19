package Lab2

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AdderTreeTest extends AnyFlatSpec with ChiselScalatestTester {

  "AdderTree test" should "pass" in {
    val adderTree_numInputs=8
    val adderTree_width=16
    test(new AdderTree(width=adderTree_width,numInputs=adderTree_numInputs)) { dut =>
        // 进行10组测试
        for(testID <- 0 until 10)
        {
            println(s"Begin test ${testID}")
            // data preparision & poke
            var trueSum: BigInt = BigInt(0)
            for(i<- 0 until adderTree_numInputs)
            {
                val rd_data=BigInt(adderTree_width, scala.util.Random)
                trueSum=trueSum+rd_data
                dut.io.inputs(i).poke(rd_data.U)
            }

            dut.io.output.expect(trueSum)

            // 前进一个Cycle
            dut.clock.step(1)
        }
        
      }
    }
}
