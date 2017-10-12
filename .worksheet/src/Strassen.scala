//object testSheet {
//  println("Welcome to the Scala worksheet")
//}

import util.Random.nextInt
import io.StdIn._

object Strassen {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(234); 
  //def Main(args:Array[String])={
    print("Enter the power factor (2^n | n is power factor): ");$skip(33); 
    val orderPower:Int=readInt();System.out.println("""orderPower  : Int = """ + $show(orderPower ));$skip(61); 
    
    val matrixOrder:Int=Math.pow(2,orderPower).intValue;System.out.println("""matrixOrder  : Int = """ + $show(matrixOrder ));$skip(87); 
    
    val matrixA = Vector.fill(matrixOrder,matrixOrder)(scala.util.Random.nextInt);System.out.println("""matrixA  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Int]] = """ + $show(matrixA ));$skip(49); 
    
    for(aElement<-matrixA)println(aElement)}
    
 // }
}
