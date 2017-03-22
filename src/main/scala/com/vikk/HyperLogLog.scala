package com.vikk

import scala.util.hashing.MurmurHash3
import scala.util.Random

/**
  * Created by vikas on 22/3/17.
  */
object HyperLogLog extends App {
  val hll = new HLL
  val values = (1 to 200000).map(i => {
    val value = Random.nextInt(16777215)
    hll.addValue(value)
    value
  })
  val actualCount = values.toSet.size
  val estimatedCount = hll.getCount()
  println(actualCount)
  println(estimatedCount)
}
