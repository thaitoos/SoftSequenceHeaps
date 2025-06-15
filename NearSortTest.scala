import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class NearSortTest {

}

object Main {
  def main(args: Array[String]): Unit = {
    val N = 1000000
    val eps = 0.07

    val heap = new SoftSequenceHeap[java.lang.Long, java.lang.Long](eps)

    val numbersToSort: ArrayBuffer[java.lang.Long] = ArrayBuffer[java.lang.Long]()

    for (i <- 1 to N) {
      val rand = Random.nextLong()

      numbersToSort.append(rand)

      heap.insert(new Element[java.lang.Long, java.lang.Long](rand, rand))
    }

    val numbersSortedByHeap: ArrayBuffer[java.lang.Long] = ArrayBuffer[java.lang.Long]()

    for (_ <- 1 to N) {
      numbersSortedByHeap.append(heap.extractMin()._1.value)
    }

    val actualNumbers = mutable.TreeMap[java.lang.Long, Int]().withDefaultValue(0)
    val heapNumbers = mutable.TreeMap[java.lang.Long, Int]().withDefaultValue(0)

    numbersToSort.foreach(x => actualNumbers(x) += 1)
    numbersSortedByHeap.foreach(x => heapNumbers(x) += 1)

    if (actualNumbers.equals(heapNumbers)) {
      println("OK - Sorted sequences have the same elements")
    } else {
      println("FAIL - Sorted sequences have different elements")
      val missingInHeap = actualNumbers.filter { case (k, v) => heapNumbers(k) != v }
      val missingInActual = heapNumbers.filter { case (k, v) => actualNumbers(k) != v }
      if (missingInHeap.nonEmpty) {
        println("Elements missing in heap sorted sequence:")
        missingInHeap.foreach { case (k, v) => println(s"Value: $k, Expected Frequency: $v, Actual Frequency: ${heapNumbers(k)}") }
      }
      if (missingInActual.nonEmpty) {
        println("Elements missing in actual sorted sequence:")
        missingInActual.foreach { case (k, v) => println(s"Value: $k, Expected Frequency: $v, Actual Frequency: ${actualNumbers(k)}") }
      }

    }

    var sortedCounts = mutable.TreeMap.empty[java.lang.Long, Int] // value -> frequency
    var inversionCount = 0L

    var  it = 0

    for (x: java.lang.Long <- numbersSortedByHeap) {
      val greaterCount = sortedCounts
        .rangeFrom(x + 1L)
        .size

      inversionCount += greaterCount

      sortedCounts.updateWith(x) {
        case Some(freq) => Some(freq + 1)
        case None => Some(1)
      }

      it += 1
      if( it % 10000 == 0) {
        println(s"Processed $it elements, current inversion count: $inversionCount")
      }
    }

    println(s"inversions : $inversionCount , eps * n^2 : ${eps * N * N}")
  }
}
