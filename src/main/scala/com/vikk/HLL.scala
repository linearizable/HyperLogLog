package com.vikk

import scala.util.hashing.MurmurHash3

/**
  * HyperLogLog algorithm for estimated counting of distinct values
  * A register is maintained which has N slots, where N = 2 ^ M
  * When a value arrives, it's hash is computed (32 bit Murmur hash)
  * This hash is then converted to it's binary representation
  * For the binary representation, rightmost M bits are taken
  * and decimal value of this M bit string is computed
  * This decimal value (Between 0 and N-1) becomes the slot number in register
  * e.g. index in register
  *
  * Then next P bits from right (leaving off rightmost M bits) are taken
  * and run of zeroes is computed from right
  * (Run of zeroes+1) becomes the value to be put into the register slot
  *
  * As multiple values arrive, a particular slot can be filled multiple times
  * We update the value in a slot only if new value is greater than the old value
  */
class HLL {
  /**
    * Number of right-most bits used to find register index (M)
    */
  val numBitsForRegisterIndex = 14

  /**
    * Size of the register e.g. no. of buckets/slots in register (N)
    */
  val registerSize = math.pow(2, numBitsForRegisterIndex).toInt

  /**
    * Number of bits used to find value in the corresponding slot in the register (P)
    */
  val numBitsForRegisterValue = 15

  val register = scala.collection.mutable.ArrayBuffer.fill(registerSize)(0)

  val alpha = registerSize match {
    case 16 => 0.673
    case 32 => 0.697
    case 64 => 0.709
    case _  => 0.7213/(1 + 1.079/registerSize)
  }

  /**
    * Add a new value to HyperLogLog
    * @param value Value to be added
    */
  def addValue(value: Int) = {
    /**
      * Create hash of the value
      * and convert to corresponding binary string
      */
    val hash = MurmurHash3.stringHash(value.toString, 11).toBinaryString

    /**
      * RegisterIndex = Decimal value of rightmost numBitsForRegisterIndex
      */
    val registerIndex = Integer.parseInt(hash.takeRight(numBitsForRegisterIndex), 2)

    /**
      * RegisterValue = Run of zeroes from right in next numBitsForRegisterValue bits + 1
      */
    val registerValue = (hash.dropRight(numBitsForRegisterIndex).takeRight(numBitsForRegisterValue).reverse.takeWhile(_ != '1').length)+1

    /**
      * Replace value in register if new value is greater than currently stored value
      */
    register(registerIndex) = if(register(registerIndex) < registerValue) registerValue else register(registerIndex)
  }

  /**
    * Get the estimated count of values seen so far by HLL algo
    * @return
    */
  def getCount(): Int = {
    val mean = harmonicMean(register)
    val countEstimate = alpha * math.pow(registerSize, 2) * mean
    correctEstimate(countEstimate).toInt
  }

  /**
    * Smooth the count estimate for very low or very high values
    * @param estimate
    * @return
    */
  def correctEstimate(estimate: Double) : Double = {

    val correctedEstimate = estimate match {
      /**
        * Low value correction
        */
      case 1 if estimate < 5/2 * registerSize => {
        val zeroRegisterCount = register.filter(_ == 0).length
        if(zeroRegisterCount > 0) registerSize * math.log(registerSize/zeroRegisterCount) else estimate
      }

      /**
        * High value correction
        */
      case 1 if estimate > (1/30 * math.pow(2, 32)) => {
        -1 * math.pow(2, 32) * math.log(1 - estimate/math.pow(2, 32))
      }

      /**
        * No correction
        */
      case _ => estimate
    }

    correctedEstimate
  }

  /**
    * Computer harmonic mean of a set of values
    * @param values
    * @return
    */
  def harmonicMean(values: Seq[Int]) : Double = {
    1/values.foldLeft(0: Double)((a, b) => a + math.pow(2, -1 * b))
  }
}