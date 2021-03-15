import scala.util.Random


class Kmeans(private var k:Int=3) {
  /**  Attribuer un cluster à chaque individus
   * de façon aléatoire.
   */
  def initClusterAleatoire(x:Array[Array[Double]],nbrlignes:Int):Array[Array[Int]]={

    var r = new Random()
    var tab = Array.ofDim[Int](nbrlignes,1)
    val min = 0
    val max = this.k-1
    var clusterAlea = 0

    for (i<-0 until nbrlignes){
      clusterAlea = min + r.nextInt(max-min+1)
      tab(i)(0)=clusterAlea
    }
    tab
  }

 /**calcule de la distance euclidienne
    */
  def distanceEuclidienne(vec1:Array[Double],vec2:Array[Double]):Double ={
    var x = 0.0
    var y = 0.0
    var d = 0.0

    for(i<-0 until vec1.length){
      x = scala.math.pow((vec1(i)-vec2(i)),2)
      y += x
    }
    d=scala.math.sqrt(y)
    return d
  }

  /** calcule de la distance euclidienne pondérée */
  def distanceEuclidiennePonderee(vec1:Array[Double],vec2:Array[Double]):Double ={
    var x = 0.0
    var y = 0.0
    var d = 0.0

    for(i <- 0 until vec1.length){
      x= scala.math.pow((vec1(i)-vec2(i)),2)
      y += x
    }
    d=(1/scala.math.sqrt(this.k))*scala.math.sqrt(y)
    return d
  }

  /** Calculer le centroïde de chaque cluster  */
  def centroide(x:Array[Array[Double]],y:Array[Array[Int]],nbrLignes:Int):Array[Array[Double]]={
    var nbrColonnes = x(0).length

    var centroide = Array.ofDim[Double](this.k,nbrColonnes)
    var z = 0
    var nbrExempleDansCluster = Array.ofDim[Int](this.k,1)

    for(i <- 0 until nbrLignes){
      z = y(i)(0)
      for (j <-0 until nbrColonnes){
        centroide(z)(j) += x(i)(j)
      }
      nbrExempleDansCluster(z)(0) += 1
    }
    for(i<-0 until this.k){
      for (j<-0 until nbrColonnes){
        centroide(i)(j)=centroide(i)(j)/nbrExempleDansCluster(i)(0)
      }
    }
    centroide
  }

  /** calcule de la distance euclidienne pondérée avec les centroides de chacun des clusters */
  def distanceCentroide(x:Array[Array[Double]],c:Array[Array[Double]],nbrLignes:Int):Array[Array[Double]]={

    var colonne = c.length
    var Dcluster = Array.ofDim[Double](nbrLignes,colonne)

    for (i<-0 until nbrLignes){
      for(j<-0 until colonne){

        Dcluster(i)(j) = distanceEuclidiennePonderee(x(i),c(j))
      }
    }
    Dcluster
  }

  /** Attribuer à chaque individus le cluster le plus proche de lui */
  def miseAJourCluster(DistFromCentre:Array[Array[Double]],y:Array[Array[Int]]): Array[Array[Int]] ={
    var ligne=DistFromCentre.length
    for(i<-0 until ligne){
      y(i)(0)=DistFromCentre(i).indexOf(DistFromCentre(i).min)
    }
    y
  }

  /** Calcule la distance euclidienne entre les centroides
   * de l iteration N et N-1 pour définir la condition d arrêt */
  def comparaisonCentroide(c:Array[Array[Double]],pastC:Array[Array[Double]]): Boolean ={
    var ligne = c.length
    var colonne = c(0).length
    var nbrCentroideStable = 0
    var D = 0.0
    var p = 0.001
    for(i<-0 until ligne){
        D = distanceEuclidienne(c(i),pastC(i))
        //p = (1/scala.math.sqrt(k))*(D)
        if(0<= D && D < p){
          nbrCentroideStable+=1
        }
    }
    if (nbrCentroideStable == this.k){
      return true
    }
    return false
  }

  /** execute l'algorithme de Kmeans   */
  def fit(x:Array[Array[Double]],iteration:Int=600):Array[Array[Int]] = {

    var nbrLignes = x.length
    var nbrColonnes = x(0).length
    var A = initClusterAleatoire(x,nbrLignes)
    var pastC = Array.ofDim[Double](this.k,nbrColonnes)

    for (i <- 0 until iteration) {
      var c = centroide(x, A,nbrLignes)
      if (comparaisonCentroide(c,pastC)){
        println("nbr iterations " + i)
        return A
      }
      pastC=c
      var D = distanceCentroide(x, c,nbrLignes)
      var g = miseAJourCluster(D, A)


    }
    println("nbr iterations "+iteration)
  return A
  }

}
