package utils

import scala.io.Source

object UtilsFunctions {

  /** Charge les classes d'Iris  */
  def getIrisClasse (label:String):Double={
    label match {
      case "Iris-setosa" =>  return 0
      case "Iris-versicolor" =>  return 1
      case "Iris-virginica" =>  return 2
      case _ => return -1
    }
  }

  /** Extraire le fichier dataIris,
   *  stocker les données dans une matrice
   * de taille (150 lignes, 5 colonnes, la dernière colonne représentant la classe réelle) */
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
            table(i)(j)=getIrisClasse(element)
          }
        }
      }
    }
    println(table(0)(4))
    println(table(50)(4))
    println(table(100)(4))
    return table
  }

  /** Extraire la colonne qui contient la classe */
  def extractClassefromData (data:Array[Array[Double]],x:Array[Array[Double]],y:Array[Array[Int]]):Unit={

    var nbrLignes = data.length
    var nbrColonnes = data(0).length
    for (i <- 0 until nbrLignes) {
      for (j <- 0 until nbrColonnes){
        if (j != nbrColonnes-1){
          x(i)(j)= data(i)(j)
        }
        else{
          y(i)(0) = data(i)(j).toInt
        }
      }
    }
  }

  /** Extraire la colonne d une matrice */
  def extractColumnfromMatrix(tab:Array[Array[Double]],ligne:Int,n:Int): Array[Double] ={
    var myArray = new Array[Double](ligne)
    for(i<-0 until ligne){
      myArray(i) = tab(i)(n)
    }
    return myArray
  }

  /** Calcule la somme d un tableau   */
  def sum(tab:Array[Double]):Double={
    var s=0.0
    for (i<-0 until(tab.length)){
      s+=tab(i)
    }
    return s
  }

 /** Calcule la moyenne d un tableau  */
  def moyenne(tab:Array[Double]):Double={
    var m=0.0
    m=(1.0/tab.length)*sum(tab)
    return m
  }

  /** Calcule de variance d un tableau */
  def variance(tab:Array[Double],m:Double,ligne:Int):Double={
    var Var=0.0
    for(i<-0 until ligne){
      Var+=(1.0/ligne)*(scala.math.pow(tab(i)-m,2))
    }
    return Var
  }

  /** Calcule de l écart type d un tableau */
  def ecartType(tab:Array[Double],m:Double,ligne:Int):Double={
    var e=0.0
    e=scala.math.sqrt( variance(tab,m,ligne))
    return e
  }

  /** Calcule de la covariance d un tableau */
  def covariance(tab:Array[Array[Double]],c1:Int,c2:Int,l:Int,m1:Double,m2:Double):Double={
    //var cov=0.0
    var som=0.0
    for(i<-0 until l){
      som+=(1.0/l)*(tab(i)(c1)*tab(i)(c2))
    }
    var cov=som-(m1*m2)
    return cov
  }

  /** Calcule de coefficient de correlation */
  def coefficientCorrelation(cov:Double,ecartType1:Double,ecartType2:Double):Double={
   var r=0.0
    r=cov/(ecartType1*ecartType2)
    return r
  }

}
