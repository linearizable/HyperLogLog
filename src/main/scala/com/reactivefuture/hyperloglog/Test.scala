package com.reactivefuture.hyperloglog

import scala.util.Random

object Test extends App {
  val hll = new HyperLogLog

  /**
    * Add 200,000 random values to hyperloglog
    */
  val values = (1 to 200000).map(i => {
    val value = Random.nextInt(16777215)
    hll.addValue(value)
    value
  })

  // Actual count of distinct values
  val actualCount = values.toSet.size

  // Estimated count of distinct values from hyperloglog
  val estimatedCount = hll.getCount
  
  println(actualCount)
  println(estimatedCount)
}
