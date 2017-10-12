import util.Random.nextInt
import io.StdIn._

object testSheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(127); 
  print("Enter the power factor (2^n | n is power factor): ");$skip(59); 
  //val orderPower:Int=readInt()
  val orderPower: Int = 1;System.out.println("""orderPower  : Int = """ + $show(orderPower ));$skip(58); 
  val matrixOrder: Int = Math.pow(2, orderPower).intValue;System.out.println("""matrixOrder  : Int = """ + $show(matrixOrder ));$skip(86); 

  val matrixA = Array.fill(matrixOrder, matrixOrder)(scala.util.Random.nextInt(20));System.out.println("""matrixA  : Array[Array[Int]] = """ + $show(matrixA ));$skip(84); 
  val matrixB = Array.fill(matrixOrder, matrixOrder)(scala.util.Random.nextInt(20));System.out.println("""matrixB  : Array[Array[Int]] = """ + $show(matrixB ));$skip(31); 
  println("Printing Matrix A");$skip(22); 
  printArray(matrixA);$skip(31); 
  println("Printing Matrix B");$skip(22); 
  printArray(matrixB);$skip(39); 

  print(sMultiply(matrixA, matrixB));$skip(106); 

  def printArray[Int](ArrayGiven: Array[Int]) = for (vElement <- ArrayGiven) println(vElement + "\t\t");System.out.println("""printArray: [Int](ArrayGiven: Array[Int])Unit""");$skip(1740); 

  def sMultiply(mA: Array[Array[Int]], mB: Array[Array[Int]]): Array[Array[Int]] = {
    val noOfRows = if (mA.length == mB.length) mA.length else 0
    var productMatrix = Array.ofDim[Int](noOfRows, noOfRows)
    if (noOfRows == 1) { // if matrix is of order 1x1
      productMatrix(0)(0) = mA(0)(0) * mB(0)(0)
      return productMatrix
    } // if matrix is of order  2^n x 2^n
    else if (math.pow(2, math.log(noOfRows) / math.log(2)) == noOfRows) {
      /*Divide array into 4 arrays*/
      val mid = noOfRows / 2

      // matrix mA
      val mAa = splitMatrix(mA, 1, mid)
      val mAb = splitMatrix(mA, 2, mid)
      val mAc = splitMatrix(mA, 3, mid)
      val mAd = splitMatrix(mA, 4, mid)

      // matrix mB
      val mBa = splitMatrix(mB, 1, mid)
      val mBb = splitMatrix(mB, 2, mid)
      val mBc = splitMatrix(mB, 3, mid)
      val mBd = splitMatrix(mB, 4, mid)

      val p1 = sMultiply(mAa, subtractMatrices(mBb, mBd))
      val p2 = sMultiply(addMatrices(mAa, mAb), mBd)
      val p3 = sMultiply(addMatrices(mAc, mAd), mBa)
      val p4 = sMultiply(mAd, subtractMatrices(mBc, mBa))
      val p5 = sMultiply(addMatrices(mAa, mAd), addMatrices(mBa, mBd))
      val p6 = sMultiply(subtractMatrices(mAb, mAd), addMatrices(mBc, mBd))
      val p7 = sMultiply(subtractMatrices(mAa, mAc), addMatrices(mBa, mBb))

      val mPa = addMatrices(subtractMatrices(addMatrices(p5, p4), p2), p6)
      val mPb = addMatrices(p1, p2)
      val mPc = addMatrices(p3, p4)
      val mPd = subtractMatrices(subtractMatrices(addMatrices(p1, p5), p3), p7)

      productMatrix = mergeMatrices(mPa, mPb, mPc, mPd)
      return productMatrix
    } else {
      println("Matrix is not of order 2^n")
      return productMatrix
    }
  };System.out.println("""sMultiply: (mA: Array[Array[Int]], mB: Array[Array[Int]])Array[Array[Int]]""");$skip(342); 

  def subtractMatrices(vA: Array[Array[Int]], vB: Array[Array[Int]]): Array[Array[Int]] = {
    val noOfRows = if (vA.length == vB.length) vA.length else 0
    val result = Array.ofDim[Int](noOfRows, noOfRows)

    for (i <- 0 until vA.length)
      for (j <- 0 until vB.length)
        result(i)(j) = vA(i)(j) - vB(i)(j)

    result
  };System.out.println("""subtractMatrices: (vA: Array[Array[Int]], vB: Array[Array[Int]])Array[Array[Int]]""");$skip(337); 

  def addMatrices(vA: Array[Array[Int]], vB: Array[Array[Int]]): Array[Array[Int]] = {
    val noOfRows = if (vA.length == vB.length) vA.length else 0
    val result = Array.ofDim[Int](noOfRows, noOfRows)

    for (i <- 0 until vA.length)
      for (j <- 0 until vB.length)
        result(i)(j) = vA(i)(j) + vB(i)(j)

    result
  };System.out.println("""addMatrices: (vA: Array[Array[Int]], vB: Array[Array[Int]])Array[Array[Int]]""");$skip(809); 

  def splitMatrix(matrixGiven: Array[Array[Int]], matrixNumber: Int, noOfRows: Int) = {
    var splittedMatrix = Array.ofDim[Int](noOfRows, noOfRows)
    var x1: Int = 0; var y1: Int = 0; var x2: Int = noOfRows; var y2: Int = noOfRows
    if (matrixNumber == 1) {
      // do nothing
    } else if (matrixNumber == 2) {
      y1 = noOfRows; y2 = matrixGiven.length
    } else if (matrixNumber == 3) {
      x1 = noOfRows; x2 = matrixGiven.length
    } else if (matrixNumber == 4) {
      y1 = noOfRows; y2 = matrixGiven.length
      x1 = noOfRows; x2 = matrixGiven.length
    }

    var x = 0; var y = 0
    for (outer <- x1 until x2) {
      y = 0
      for (inner <- y1 until y2) {
        splittedMatrix(x)(y) = matrixGiven(outer)(inner)
        y += 1
      }
      x += 1
    }
    splittedMatrix
  };System.out.println("""splitMatrix: (matrixGiven: Array[Array[Int]], matrixNumber: Int, noOfRows: Int)Array[Array[Int]]""");$skip(819); 

  def mergeMatrices(mA: Array[Array[Int]], mB: Ar ray[Array[Int]], mC: Array[Array[Int]], mD: Array[Array[Int]]) = {
    val noOfRows = mA.length * 2
    val HalfNoOfRows = mA.length
    val mergedMatrix = Array.ofDim[Int](noOfRows, noOfRows)
    for (outer <- 0 until noOfRows) {
      for (inner <- 0 until noOfRows) {
        if (outer >= HalfNoOfRows && inner >= HalfNoOfRows) {
          mergedMatrix(outer)(inner) = mD(outer - HalfNoOfRows)(inner - HalfNoOfRows)
        } else if (outer >= HalfNoOfRows) {
          mergedMatrix(outer)(inner) = mC(outer - HalfNoOfRows)(inner)
        } else if (inner >= HalfNoOfRows) {
          mergedMatrix(outer)(inner) = mB(outer)(inner - HalfNoOfRows)
        } else {
          mergedMatrix(outer)(inner) = mA(outer)(inner)
        }
      }
    }
    mergedMatrix
  };System.out.println("""mergeMatrices: (mA: Array[Array[Int]], mB: <error>)Unit""")}

}
