##-500 - 500
schweffel <- function(xx)
{
  d <- length(xx)
  
  sum <- sum(xx*sin(sqrt(abs(xx))))
  
  y <- 418.98288727216*d - sum
  return(y)
}

schweffelSurface <- function(x,y)
{
  d <- 2
  sum <- (x*sin(sqrt(abs(x))))+(y*sin(sqrt(abs(y))))
  
  
  yy <- 418.98288727216*d - sum
  return(yy)
}



##-100 - 100
easom <- function(xx)
{
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1 <- -cos(x1)*cos(x2)
  fact2 <- exp(-(x1-pi)^2-(x2-pi)^2)
  
  y <- fact1*fact2
  return(y)
}

easomSurface <- function(x,y)
{
  x1 <- x
  x2 <- y
  
  fact1 <- -cos(x1)*cos(x2)
  fact2 <- exp(-(x1-pi)^2-(x2-pi)^2)
  
  y <- fact1*fact2
  return(y)
}


##-5 s.d 5
rosenbrock <- function(xx)
{ 
  d <- length(xx)
  xi <- xx[1:(d-1)]
  xnext <- xx[2:d]
  
  sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)
  
  y <- sum
  return(y)
}

rosenbrockSurface <- function(x,y)
{ 
  d <- 2
  xi <- x
  xnext <- y
  
  sum <- 100*(xnext-xi^2)^2+(xi-1)^2
    
  y <- sum
  return(y)
}



##rastr [-5.12, 5.12]
rastrigin <- function(xx)
{  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}


##rastr [-5.12, 5.12]
rastriginSurface <- function(x,y)
{  
  d <- 2
  
  sum <- (x^2-10*cos(2*pi*x)) -(y^2-10*cos(2*pi*y))
  
  y <- 10*d + sum
  return(y)
}


##bencmark goldstein
goldstein<-function(x){
  first=(1.0+(x[1]+x[2]+1.0)*(x[1]+x[2]+1.0)*(19.0-14.0*x[1]+3.0*x[1]*x[1]-14.0*x[2]+6.0*x[1]*x[2]+3.0*x[2]*x[2]));	
  second=30.0+(2.0*x[1]-3.0*x[2])*(2.0*x[1]-3.0*x[2])*(18.0-32.0*x[1]+12.0*x[1]*x[1]+48.0*x[2]-36.0*x[1]*x[2]+27*x[2]*x[2]);	
  goldstein=first*second;	
}

##bencmark goldstein
goldsteinSurface<-function(x,y){
  first=(1.0+(x+y+1.0)*(x+y+1.0)*(19.0-14.0*x+3.0*x*x-14.0*y+6.0*x*y+3.0*y*y));	
  second=30.0+(2.0*x-3.0*y)*(2.0*x-3.0*y)*(18.0-32.0*x+12.0*x*x+48.0*y-36.0*x*y+27*y*y);	
  goldstein=first*second;	
}


deJong<-function(x){ 
  deJong = sum(x^2)
}

deJongSurface<-function(x,y){ 
  deJong = x^2+y^2
  return (deJong)
}


##benchmark griewank function -600<x<600 dua dimensi
griewank <- function(xx){
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  return(y)
}

##benchmark griewank function -600<x<600 dua dimensi
griewankSurface <- function(x,y){
  sum <- x^2/4000+y^2/4000
  prod <- cos(x/sqrt(1))*cos(y/sqrt(2))
  
  z <- sum - prod + 1
  return(z)
}


FPAModulVis<-function(N=25,p=0.8,delta=1.5,eta=0.1,maxiter=20000,LowerV=c(0,0,0),UpperV=c(1000,1000,1000),FUN=SVRFit,objFit=0.00001,global=0,randEta=F){
  ##validasi fungsi utama
  xX = seq(LowerV[1],UpperV[1],length.out = 100)
  yY = seq(LowerV[2],UpperV[2],length.out = 100)
  FUNsurf = getFunction(paste0(FUN,"Surface"))
  zZ = outer(xX,yY,FUNsurf)
  FUN = getFunction(FUN)
  thisCol = RColorBrewer::brewer.pal(9,"Blues")
  filled.contour(xX,yY,zZ, color.palette = colorRampPalette(thisCol))
  
  dataIter = c()
  if(length(UpperV)!=length(LowerV)) {
    FPAModul=FALSE
  }
  else{
    D = length(UpperV) ##variabel dimensi dari parameter yang akan dioptimasi
    GBest = vector(mode = "numeric",length = D) ##variabel untuk menyimpan posisi nilai terbaik 
    Fitness = vector(mode = "numeric",length = N) 
    XX = matrix(0,nrow = N,ncol = D)
    YY = matrix(0,nrow = N,ncol = D)
    
    ##inisiasi nilai awal untuk setiap bunga pada rentang nilai parameter yang telah ditentukan
    init<-function(){
      temp=1/0
      minvalIndex = 0
      for(i in 1:N){
        for(j in 1:D){
          XX[i,j]=LowerV[j]+(UpperV[j]-LowerV[j])*runif(1)
        }
      }
      
      ##find current best
      for(i in 1:N){
        tempA = FUN(XX[i,])
        Fitness[i] = tempA
        if(temp>tempA){
          temp = tempA 
          minvalIndex = i
        }
      }
      
      
      ##print(Fitness)
      GBest = XX[minvalIndex,]
      init=list(temp,GBest,XX,Fitness) ##return fitness Value
    }
    
    simpleBounds<-function(x,LowerV,UpperV){
      ##print("before")
      ##print(x)
      for(i in 1:length(x)){
        if(x[i]<LowerV[i]) x[i]=LowerV[i]
        if(x[i]>UpperV[i]) x[i]=UpperV[i]
      }
      ##print("after")
      ##print(x)
      simpleBounds = x
    }
    
    solution<-function(){
      initA = init()
      FitnessValue=initA[[1]] ##getVitness Value from init
      ##print(FitnessValue)
      GBest = initA[[2]]
      XX = initA[[3]]
      Fitness = initA[[4]]
      YY=XX
      
      ##print(Fitness)
      ##print(FitnessValue)
      sigma = (gamma(1+delta)*sin(pi*delta/2)/(gamma((1+delta)/2)*delta*2^((delta-1)/2)))^(1/delta)
      
      iter = 0
      
      cplot = c()
      
      while (iter<=maxiter && abs(global-FitnessValue)>objFit) {
        iter=iter+1
        for(i in 1:N){
          if(runif(n = 1,min = 0,max = 1)<p){ ##Global pollination
            V = rnorm(n = D,mean = 0,sd = 1)
            U = rnorm(n = D,mean = 0,sd = sigma)
            step = U/(abs(V)^(1/delta))
            for(j in 1:D){
              if(randEta) YY[i,j]=XX[i,j]+abs(rnorm(1,mean=0,sd=eta))*step[j]*(GBest[j]-XX[i,j])  ## Levy Flight
              else YY[i,j]=XX[i,j]+eta*step[j]*(GBest[j]-XX[i,j])  ## Levy Flight
            }
            
          }
          else{ ##local pollination
            eps = runif(n = 1,min = 0,max = 1) ##epsilon value from uniform distribution
            
            indexJK = sample(1:N) ##lakukan permutasi untuk memilih dua bunga dari N bunga secara acak
            while(indexJK[1]==i || indexJK[2]==i){
              indexJK = sample(1:N) ##lakukan permutasi untuk memilih dua bunga dari N bunga secara acak
            }
            for(j in 1:D){
              YY[i,j]=XX[i,j]+eps*(XX[indexJK[1],j]-XX[indexJK[2],j])
            }
            
          }
          YY[i,] = simpleBounds(YY[i,],LowerV,UpperV)
          Fnew = FUN(YY[i,])
          ##print(Fnew)
          if(Fnew<Fitness[i]){
            Fitness[i] = Fnew
            XX[i,]=YY[i,]
          }
          
          if(Fnew<FitnessValue){
            FitnessValue = Fnew
            GBest = YY[i,]
          }  
        }
        
        cplot=c(cplot,abs(global-FitnessValue))
        
        
        dataIter = c(dataIter,FitnessValue)
        
        if(iter%%2==0){
          print(iter)
          print(GBest)
          print(FitnessValue)
          # mm = max(dataIter)
          #print(GBest[1])
          png(filename=paste0("www/plot",iter,".png"))
          filled.contour(xX,yY,zZ,plot.axes = {points(XX[,1],XX[,2],pch=19,col=(1:N),cex=2);axis(1,pretty(xX));axis(2,pretty(yY))},color.palette = colorRampPalette(thisCol),plot.title = title(main = paste0("iteration number: ",iter),xlab = "X1",ylab = "X2"))
          dev.off()
          
          # Sys.sleep(2)
          # plot(cplot,ylim = range(0,mm),xlab = "Iteration",ylab = "Objective function", main = "FPA Optimization")
        }
        
      }
      solution = list(GBest,FitnessValue,iter,dataIter)
    }
    
    FPAModul=solution()
  }
} 

