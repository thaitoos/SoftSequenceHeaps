import SoftSequenceHeap.mergeByRank

import scala.collection.mutable.ArrayDeque
import scala.math.ceil
import scala.util.Random

private class Node[T](var value: T, var next: Node[T])
class LinkedList[T] {
  // Internal node class
  private var head: Node[T] = null
  private var tail: Node[T] = null
  private var size: Int = 0

  // Add element to the end
  def enqueue(value: T): Unit = {
    val node = new Node(value, null)
    if (tail == null) {
      head = node
      tail = node
    } else {
      tail.next = node
      tail = node
    }
    size += 1
  }

  // Remove and return the first element
  def dequeue(): T = {
    if (head == null) throw new NoSuchElementException("Queue is empty")
    val value = head.value
    head = head.next
    if (head == null) tail = null
    size -= 1
    value
  }

  // Look at the first element without removing
  def peek: T = {
    if (head == null) throw new NoSuchElementException("Queue is empty")
    head.value
  }

  // Merge another queue into this one (destructive, constant time)
  def merge(that: LinkedList[T]): LinkedList[T] = {
    if (that.isEmpty) return this

    if (this.isEmpty) {
      this.head = that.head
      this.tail = that.tail
    } else {
      this.tail.next = that.head
      this.tail = that.tail
    }
    this.size += that.size

    // Clear the other queue
    that.head = null
    that.tail = null
    that.size = 0
    this
  }

  def isEmpty: Boolean = head == null

  // Debugging: convert to list for inspection
  def toList: List[T] = {
    var curr = head
    var acc = List.empty[T]
    while (curr != null) {
      acc = curr.value :: acc
      curr = curr.next
    }
    acc.reverse
  }
}

class PrunedElement [T <: Comparable[T], S](val key: T, val value: S) {
  def getKey: T = key
  def getValue: S = value
}

object PrunedElement {
  def apply[T <: Comparable[T], S](key: T, value: S): PrunedElement[T, S] = new PrunedElement(key, value)
}

class Element[T <: Comparable[T], S](val key: T, val value: S) {
  var corruptionSet = new LinkedList[PrunedElement[T, S]]()
  var witnessSet = new LinkedList[PrunedElement[T, S]]()
  def getKey: T = key
  def getValue: S = value
}

class Sequence[T <: Comparable[T], S] {
  var elements = new ArrayDeque[Element[T, S]]()
  var suffixMin : Sequence[T, S] = this
  var rank = 0
  def getHead: Element[T, S] = elements.head // Assuming elements is not empty
  def addElement(element: Element[T, S]): Unit = elements.append(element)
  def getSuffixMin: Sequence[T, S] = suffixMin
  def getRank: Int = rank
  def increaseRank(): Unit = rank += 1
  def setRank(newRank: Int): Unit = rank = newRank

  def reduce(): Unit = {
    val newElements = new ArrayDeque[Element[T, S]]()
    var i = 0
    while(i < elements.size) {
      val currentElement = elements(i)
      if(i % 2 == 0 || i == elements.size - 1) {
        newElements.append(currentElement)
      } else {
        newElements(i/2).witnessSet.merge(currentElement.witnessSet).enqueue(new PrunedElement[T, S](currentElement.getKey, currentElement.getValue))
        elements(i+1).corruptionSet.merge(currentElement.corruptionSet).enqueue(new PrunedElement[T, S](currentElement.getKey, currentElement.getValue))
      }
      i += 1
    }
    elements = newElements
  }

  def updateSuffixMin(nextSequence: Sequence[T,S]): Unit = {
    if(nextSequence == this) {
      suffixMin = this
      return
    }
    val candidateSuffixMin = nextSequence.suffixMin
    if(this.getHead.getKey.compareTo(candidateSuffixMin.getHead.getKey) > 0) {
      suffixMin = nextSequence.suffixMin
    } else {
      suffixMin = this
    }
  }
}

object Sequence {
  def apply[T <: Comparable[T], S](element: Element[T, S]): Sequence[T, S] = {
    val sequence = new Sequence[T, S]
    sequence.addElement(element)
    sequence
  }

  def merge[T <: Comparable[T], S](seq1: Sequence[T, S], seq2: Sequence[T, S]): Sequence[T, S] = {
    val mergedSequence = new Sequence[T, S]
    while (!seq1.elements.isEmpty || !seq2.elements.isEmpty) {
      if (seq1.elements.isEmpty) {
        mergedSequence.addElement(seq2.elements.removeHead())
      } else if (seq2.elements.isEmpty) {
        mergedSequence.addElement(seq1.elements.removeHead())
      } else {
        if (seq1.getHead.getKey.compareTo(seq2.getHead.getKey) < 0) {
          mergedSequence.addElement(seq1.elements.removeHead())
        } else {
          mergedSequence.addElement(seq2.elements.removeHead())
        }
      }
    }
    mergedSequence
  }
}

class SoftSequenceHeap[T <: Comparable[T], S] (val epsilon: Double) {
  val r0 = ceil(1.0 / epsilon).toInt
  var sequences = new ArrayDeque[Sequence[T, S]]()

  def extractMin(): (PrunedElement[T, S], T, LinkedList[PrunedElement[T,S]]) = {
    val minSequence = sequences.head.suffixMin
    val k = minSequence.getHead.getKey
    if(!minSequence.getHead.corruptionSet.isEmpty) {
      val returnedElement = minSequence.getHead.corruptionSet.dequeue()
      return (PrunedElement[T,S](returnedElement.getKey, returnedElement.getValue), k, null)
    } else {
      val returnedElement = minSequence.getHead
      val minSequenceIndex = sequences.indexOf(minSequence)
      if(minSequence.elements.length == 1) {
        sequences.remove(minSequenceIndex)
      } else {
        minSequence.elements.removeHead()
      }
      for (i <- (0 to Math.min(minSequenceIndex, sequences.length - 2)).reverse) {
        sequences(i).updateSuffixMin(sequences(i + 1))
      }

      if(sequences.length >= 1) {
        sequences(sequences.length - 1).updateSuffixMin(sequences(sequences.length - 1))
      }

      return (PrunedElement[T,S](returnedElement.getKey, returnedElement.getValue), k, returnedElement.witnessSet)
    }
  }

  def insert(element: Element[T, S]): Unit = {
    val newSequence = Sequence(element)
    sequences.prepend(newSequence)
    while (sequences.length >= 2 && sequences(0).getRank.compareTo(sequences(1).getRank) == 0) {
      val mergedSequence = Sequence.merge(sequences(0), sequences(1))
      val previousRank = sequences(0).getRank
      sequences.remove(0)
      sequences.remove(0)
      sequences.prepend(mergedSequence)
      mergedSequence.setRank(previousRank + 1)
      if(sequences.head.getRank > r0 && (sequences.head.getRank - r0) % 2 == 0) {
        sequences.head.reduce()
      }
    }
    if (sequences.length > 1) {
      sequences(0).updateSuffixMin(sequences(1))
    } else {
      sequences(0).updateSuffixMin(sequences(0))
    }
  }

  def meld(that: SoftSequenceHeap[T, S]): SoftSequenceHeap[T, S] = {
    sequences = mergeByRank(sequences, that.sequences)
    var i = sequences.length - 2
    var foundMatch = true
    while (foundMatch) {
      foundMatch = false
      while (i >= 0 && !foundMatch) {
        if (sequences(i).getRank == sequences(i + 1).getRank) {
          val mergedSequence = Sequence.merge(sequences(i), sequences(i + 1))
          val newRank = sequences(i).getRank + 1
          sequences.remove(i + 1)
          sequences.remove(i)
          sequences.insert(i, mergedSequence)
          mergedSequence.setRank(newRank)

          if (mergedSequence.getRank > r0 && (mergedSequence.getRank - r0) % 2 == 0) {
            mergedSequence.reduce()
          }

          foundMatch = true
        }
        i -= 1
      }
    }

    // Update suffix minimums
    for (j <- (0 until sequences.length).reverse) {
      if (j == sequences.length - 1) {
        sequences(j).updateSuffixMin(sequences(j))
      } else {
        sequences(j).updateSuffixMin(sequences(j + 1))
      }
    }

    this
  }

}

object SoftSequenceHeap {
  def mergeByRank[T <: Comparable[T], S](seq1: ArrayDeque[Sequence[T, S]], seq2: ArrayDeque[Sequence[T, S]]): ArrayDeque[Sequence[T, S]] = {
    val merged = new ArrayDeque[Sequence[T, S]]()
    var i = 0
    var j = 0

    while (i < seq1.length && j < seq2.length) {
      if (seq1(i).getRank < seq2(j).getRank) {
        merged.append(seq1(i))
        i += 1
      } else {
        merged.append(seq2(j))
        j += 1
      }
    }

    while (i < seq1.length) {
      merged.append(seq1(i))
      i += 1
    }

    while (j < seq2.length) {
      merged.append(seq2(j))
      j += 1
    }

    merged
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val heap = new SoftSequenceHeap[java.lang.Long, String](-1)

    val random = new Random()
    val n = 30
    val input = (1L to n).map(i => {
      val key = random.nextLong(50)
      new Element[java.lang.Long, String](key, s"val_$key")
    }).toList

    println("Inserting elements (unsorted):")
    input.foreach(e => {
      println(s"Inserting: ${e.getKey} -> ${e.getValue}")
      heap.insert(e)
    })

    val expected = input.map(e => e.getKey).sorted
    val actual = collection.mutable.ListBuffer[java.lang.Long]()

    println("\nExtracting elements in order:")
    for (_ <- 1 to n){
      try {
        val (minElement, minKey, witnessSet) = heap.extractMin()
        println(s"Extracted: ${minElement.getKey} -> ${minElement.getValue}, Witness Set: ${if (witnessSet != null) witnessSet.toList.mkString(", ") else "None"}")
        actual += minElement.getKey
      } catch {
        case _: NoSuchElementException =>
          println("Heap is empty.")
      }
    }

    println("\nExpected sorted keys:")
    println(expected.mkString(", "))

    println("\nActual extracted keys:")
    println(actual.mkString(", "))
  }
}
