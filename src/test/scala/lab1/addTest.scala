/**
 * This code is a test for an AdderModule. It generates random test data, feeds it into the module, 
 and checks if the output is correct. The test is performed 10 times.
 *
 * @file /root/chisel-template/src/test/scala/addTest.scala
 */

// Define a package named Adder
package Adder

// Import necessary libraries
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// Define a class named AdderTest that extends（继承） AnyFlatSpec and ChiselScalatestTester
class AdderTest extends AnyFlatSpec with ChiselScalatestTester {

  // Define a test named "Adder test" that should pass
  "Adder test" should "pass" in {
      
    // instantiate a new Uint64_Adder and test it
    test(new Uint64_Adder) { dut =>   // dut: device under test, refer instance of Uint64_Adder
      // 进行10组测试
      for(testID <- 0 until 10){
        println(s"Begin test ${testID}")
        // 准备测试数据 2^16以下的随机数
        val a=scala.util.Random.nextInt(0xFFFF)
        val b=scala.util.Random.nextInt(0xFFFF)
        // 计算生成正确结果(ground truth) 
        val c=a+b
        //  ${} syntax used to embed expressions within string. 
        println(s"a=0x${a.toHexString} b=0x${b.toHexString} c=0x${c.toHexString}")

        // 将测试数据输入到模块
        dut.io.in_a.poke(a)  // poke is chisel test method to drive a value onto hardware node
        dut.io.in_b.poke(b)
        // 检查模块输出是否正确
        dut.io.out_c.expect(c)

        // 前进一个Cycle， 进行下一个测试
        dut.clock.step(1)
      }        
    }

    // Another way to conduct the above test
    test(new Uint64_Adder) { dut =>
    // Perform 10 test cases
    for (testID <- 10 until 20) {
      println(s"Begin test ${testID}")
      // Generate random test data within the range of 0 to 2^64 - 1
      val a = BigInt(64, scala.util.Random)
      val b = BigInt(64, scala.util.Random)

      // Calculate the expected result
      // perform addition and a bitwise AND, to ensure the result is within the range of 0 to 2^64 - 1
      val c = (a + b) & BigInt("FFFFFFFFFFFFFFFF", 16) //16代表进制
      println(s"a=0x${a.toString(16)} b=0x${b.toString(16)} c=0x${c.toString(16)}") // calling the toString in base 16 (hex)

      // Set the input values
      dut.io.in_a.poke(a.U)
      dut.io.in_b.poke(b.U)
      // Check if the output matches the expected result
      dut.io.out_c.expect(c.U)

      // Advance the clock by one cycle
      dut.clock.step(1)
    }
    }   
  }

}