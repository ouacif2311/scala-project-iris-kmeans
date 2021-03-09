package utils

import scala.io.Source

object UtilsFunctions {

  def getirisclasse (label:String):Double={
    label match {
      case "Iris-setosa" =>  return 0
      case "Iris-versicolor" =>  return 1
      case "Iris-virginica" =>  return 2
      case _ => return -1
    }
  }

  def dataFromFileToMatrix(ligne:Int,colonne:Int,fileSource:String):Array[Array[Double]]={
    val table=Array.ofDim[Double](ligne,colonne)
    val filename = "fileopen.scala"
    for ((line,i) <- Source.fromFile(fileSource).getLines.zipWithIndex) {
      //println(s"$i:$line")
      if (i<ligne){
        val x= line split(",")
        //println(x.length)
        for((element,j)<- x.zipWithIndex){
          if(j<(colonne-1)){
            table(i)(j)=element.toDouble
          }
          else{
            table(i)(j)=getirisclasse(element)
          }
        }
      }
    }
    println(table(0)(4))
    println(table(50)(4))
    println(table(100)(4))
    return table
  }

  def som(tab:Array[Double]):Double={
    var s=0.0
    for (i<-0 until(tab.length)){
      s+=tab(i)
    }
    return s
  }

  def moy(tab:Array[Double]):Double={
    var m=0.0
    m=(1.0/tab.length)*som(tab)
    return m
  }

  def matrixToTab(tab:Array[Array[Double]],ligne:Int,n:Int): Array[Double] ={
    var myArray = new Array[Double](ligne)
    for(i<-0 until ligne){
      myArray(i) = tab(i)(n)
    }
    return myArray
  }

  def variance(tab:Array[Double],m:Double,ligne:Int):Double={
    var Var=0.0
    for(i<-0 until ligne){
      Var+=(1.0/ligne)*(scala.math.pow(tab(i)-m,2))
    }
    return Var
  }

  def ecartType(tab:Array[Double],m:Double,ligne:Int):Double={
    var e=0.0
    e=scala.math.sqrt( variance(tab,m,ligne))
    return e
  }

  def covariance(tab:Array[Array[Double]],c1:Int,c2:Int,l:Int,m1:Double,m2:Double):Double={
    //var cov=0.0
    var som=0.0
    for(i<-0 until l){
      som+=(1.0/l)*(tab(i)(c1)*tab(i)(c2))
    }
    var cov=som-(m1*m2)
    return cov
  }
}
