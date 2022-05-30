#Kolmogorov Smirnov Test
lcg=function(a,c,m,n,X0){
  X = c()
  X[1] = X0
  Xn = X[1]
  Xn = X0
  for (i in 2:n) {
    Xn = (a*Xn + c) %% m
    X[i] = Xn
  }
  #  X = X/m
  return(X)
}

#Example 1
a=lcg(5,1,16,15,1)
y=a/16
ks.test(y,"punif",0,1)


#Example2
#RANDU Generator
b=lcg(65539,0,2^31,15000,1)
x=b/2^31
ks.test(x,"punif",0,1)




#Runs Test Examples


# Q1 of run test
library(randtests)
b=runif(30,0,1)
b
runs.test(b,plot=TRUE)

# Q2 of run test
lcg = function(a,c,m,n,seed){
  x = rep(0,n)
  x[1] = seed
  for(i in 1:(n-1)){
    x[i+1] = (a*x[i]+c)%%m
  }
  U=x/m
  return(list(U))
}
a = lcg(11,37,20,50,3)
a
runs.test(x)


# Q3 of runs test
d=runif(50,0,1)
d
runs.test(d)




#LinearCongreuntialGenerator

LCG <- function(a,x0,c,m,nSim){
  RndN = c()
  for(i in 1:nSim){
    x1 =(a*x0+c)%%m
    RndN[i]<-x1
    x0=x1
  }
  assign("nSim",nSim,envir =.GlobalEnv)
  assign("RndN",RndN,envir =.GlobalEnv)
}

#Pokertest

pokertest <- function(u){
  ExpFrq=length(u)*c(0.72,0.27,0.01)
  identical=0
  pair=0
  x=strsplit(format(round(u,3)),"")
  for(i in 1:length(x)){
    if (x[[i]][3]==x[[i]][4] & x[[i]][4]==x[[i]][5]){
      identical=identical+1
    }
    else if(x[[i]][3]==x[[i]][4]|x[[i]][4]==x[[i]][5]|x[[i]][3]==x[[i]][5]){
      pair=pair+1
    }
  }
  ObsFreq=c(length(u)-identical-pair,pair,identical)
  ExpFreq=length(u)*c(0.72,0.27,0.01)
  chi.sq.cal=sum(((ObsFreq-ExpFreq)^2)/ExpFreq)
  print(paste(c("ObsFreqOfDifferent :","ObsFreqOfPair :","ObsFreqOfIdentical :"),ObsFreq))
  print(paste(c("ExpFreqOfDifferent :","ExpFreqOfPair :","ExpFreqOfIdentical :"),ExpFreq))
  print(paste("chi.sq.cal :",round(chi.sq.cal,3)))
  print(paste("p-value",pchisq(chi.sq.cal, df=2, lower.tail=FALSE)))
  
  
}


#Poker Test Examples

#First Example

x=c(0.266,0.372,0.573,0.908,0.202,0.898,0.945,0.661,0.629,0.062,0.206,0.177,0.687,0.384,0.770,0.498,0.718,0.992,0.380,0.777,0.935,0.212,0.652,0.126,0.267,0.386,0.013,0.382,0.870,0.340)
pokertest(x)

#Second Example

LCG(3,15,0,31,31)
RndN = round(RndN/31,3)
pokertest(RndN)


#Third Example
set.seed(111)
y=round(runif(1000),3)
pokertest(y)


#SpectralTestPlots

#First Example
LCG(3,15,0,31,31)
RndN
i=1:nSim
plot(RndN[i],RndN[i+1])

#Second Example
LCG(13,15,0,31,31)
RndN
i=1:nSim
plot(RndN[i],RndN[i+1])


#RANDU Eg
LCG(65539,1,0,2^31,15000)
i=1:nSim
RndN = RndN/2^31
library(rgl)
plot3d(RndN[i],RndN[i+1],RndN[i+2],col = "red")

