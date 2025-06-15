import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class NearSortTest {

}

object main {
  val N = 1000000
  val eps = 0.05

  val heap = new SoftSequenceHeap[java.lang.Long, java.lang.Long](eps)

  val numbersToSort: ArrayBuffer[java.lang.Long] = ArrayBuffer[java.lang.Long]()

  for (i <- 1 to N) {
    val rand = Random.nextLong()

    numbersToSort.append(rand)

    heap.insert(new Element[java.lang.Long, java.lang.Long](rand, rand))
  }

  val numbersSortedByHeap: ArrayBuffer[java.lang.Long] = ArrayBuffer[java.lang.Long]()

  for(_ <- 1 to N) {
    numbersSortedByHeap.append(heap.extractMin()._1.value)
  }

  val actualNumbers = mutable.TreeMap[java.lang.Long, Int]()
  val heapNumbers = mutable.TreeMap[java.lang.Long, Int]()

  numbersToSort.foreach(x => actualNumbers(x) += 1)
  numbersSortedByHeap.foreach(x => heapNumbers(x) += 1)

  if(numbersToSort.equals(numbersSortedByHeap)) {
    println("OK - Sorted sequences have the same elements")
  } else {
    println("FAIL - Sorted sequences have different elements")
  }

  var sortedCounts = mutable.TreeMap.empty[java.lang.Long, Int] // value -> frequency
  var inversionCount = 0L

  for (x: java.lang.Long <- numbersSortedByHeap) {
    val greaterCount = sortedCounts
      .rangeFrom(x + 1L)
      .values
      .sum

    inversionCount += greaterCount

    sortedCounts.updateWith(x) {
      case Some(freq) => Some(freq + 1)
      case None       => Some(1)
    }
  }

  println(s"inversions : $inversionCount , eps * n^2 : $$${eps * N * N}")
  
  

}
