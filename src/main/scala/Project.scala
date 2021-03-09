import utils.UtilsFunctions

object Project {

  def main (args:Array[String])
  {
    val L=150
    val C=5

    var datairis=Array.ofDim[Double](L,C)
    var SepalLength = new Array[Double](L)
    var SepalWidth = new Array[Double](L)
    var PetalLength = new Array[Double](L)
    var PetalWidth = new Array[Double](L)

    datairis=UtilsFunctions.dataFromFileToMatrix(L,C,"src/ressources/iris.data")

    for(i<-0 until L)
    {
      for(j <-0 until C-1) {
        print(datairis(i)(j) + ", ")
      }
      println()
    }
    /*for (i<-0 until L){
      print(moyArray(i )+ ",")}*/
    println()
    SepalLength=UtilsFunctions.matrixToTab(datairis,L,0)
    var m = UtilsFunctions.moy(SepalLength)
    println("la moyenne SepalLength:"+" "+ m)
    //println("on verra "+ 1.0/150)
    SepalWidth=UtilsFunctions.matrixToTab(datairis,L,1)
    var m1 = UtilsFunctions.moy(SepalWidth)
    println("la moyenne SepalWidth:"+" "+ m1)
    PetalLength=UtilsFunctions.matrixToTab(datairis,L,2)
    var m2 = UtilsFunctions.moy(PetalLength)
    println("la moyenne PetalLength:"+" "+ m2)
    PetalWidth=UtilsFunctions.matrixToTab(datairis,L,3)
    var m3 = UtilsFunctions.moy(PetalWidth)
    println("la moyenne PetalWidth:"+" "+ m3)
    println()
    var v=UtilsFunctions.variance(SepalLength,m,L)
    println("la variance SepalLength:"+" "+ v)
    var v1=UtilsFunctions.variance(SepalWidth,m1,L)
    println("la variance SepalWidth"+" "+v1)
    var v2=UtilsFunctions.variance(PetalLength,m2,L)
    println("la variance PetalLength"+" "+v2)
    var v3=UtilsFunctions.variance(PetalWidth,m3,L)
    println("la variance PetalWidth"+" "+v3)
    println()
    var e=UtilsFunctions.ecartType(SepalLength,m,L)
    println("l'écart-type SepalLength:"+" "+ e)
    var e1=UtilsFunctions.ecartType(SepalWidth,m1,L)
    println("l'écart-type SepalWidth:"+" "+ e1)
    var e2=UtilsFunctions.ecartType(PetalLength,m2,L)
    println("l'écart-type PetalLength:"+" "+ e2)
    var e3=UtilsFunctions.ecartType(PetalWidth,m3,L)
    println("l'écart-type PetalWidth:"+" "+ e3)
    println()
    var cov=UtilsFunctions.covariance(datairis,0,1,L,m,m1)
    println("la cov yy1:"+" "+cov)
    var cov1=UtilsFunctions.covariance(datairis,0,2,L,m,m2)
    println("la cov yy2:"+" "+cov1)
    var cov2=UtilsFunctions.covariance(datairis,0,3,L,m,m3)
    println("la cov yy3:"+" "+cov2)
    var cov3=UtilsFunctions.covariance(datairis,1,2,L,m1,m2)
    println("la cov y1y2:"+" "+cov3)
    var cov4=UtilsFunctions.covariance(datairis,1,3,L,m1,m3)
    println("la cov y1y3:"+" "+cov4)
    var cov5=UtilsFunctions.covariance(datairis,2,3,L,m2,m3)
    println("la cov y2y3:"+" "+cov5)
    println()
    var r=cov/(e*e1)
    println("la coeﬀicient de corrélation yy1:"+" "+r)
    var r1=cov1/(e*e2)
    println("la coeﬀicient de corrélation yy2:"+" "+r1)
    var r2=cov2/(e*e3)
    println("la coeﬀicient de corrélation yy3:"+" "+r2)
    var r3=cov3/(e1*e2)
    println("la coeﬀicient de corrélation y1y2:"+" "+r3)
    var r4=cov4/(e1*e3)
    println("la coeﬀicient de corrélation y1y3:"+" "+r4)
    var r5=cov5/(e2*e3)
    println("la coeﬀicient de corrélation y2y3:"+" "+r5)
  }

}
