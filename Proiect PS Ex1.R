#Problema 1

#Ex 1 media si varianta 1000 v.a. independente

p = rpois(1000,5.2)
b = rbinom(1000, 10, 0.1)
e = rexp(1000,10)
n = rnorm(1000,5)
print(var(p))
print(mean(p))
print(var(b))
print(mean(b))
print(var(e))
print(mean(e))
print(var(n))
print(mean(n))

#Ex 2 si 3 graficele functiilor de masa, densitate si functiile de repartitie (distributie)

##graficele poisson
p1 = data.frame(DENSITATE=dpois(0:20, 0.1), MASA=ppois(0:20, 0.1), DISTRIBUTIE= rpois(0:20, 0.1))
p2 = data.frame(DENSITATE=dpois(0:20, 2), MASA=ppois(0:20, 2), DISTRIBUTIE= rpois(0:20, 2))
p3 = data.frame(DENSITATE=dpois(0:20, 5), MASA=ppois(0:20, 5), DISTRIBUTIE= rpois(0:20, 5))
p4 = data.frame(DENSITATE=dpois(0:20, 500), MASA=ppois(0:20, 500), DISTRIBUTIE= rpois(0:20, 500))
p5 = data.frame(DENSITATE=dpois(0:20, 0.01), MASA=ppois(0:20, 0.01), DISTRIBUTIE= rpois(0:20, 0.01))

plot(p1$DENSITATE, type="o",col="red", main="Poisson Densitate") 
lines(p2$DENSITATE,Type="o",col="green")
lines(p3$DENSITATE,Type="o",col="blue")
lines(p4$DENSITATE,Type="o",col="yellow")
lines(p5$DENSITATE,Type="o",col="coral")
legend("topright",c("l=0.1","l=2","l=5","l=500", "l=0.01"), col=c("red","green","blue","yellow","coral"),pch=15)

plot(p1$MASA,type="o",col="red", main="Poisson Masa") 
lines(p2$MASA,type="o",col="green")
lines(p3$MASA,type="o",col="blue")
lines(p4$MASA,type="o",col="yellow")
lines(p5$MASA,type="o",col="coral")
legend("bottomright",c("l=0.1","l=2","l=5","l=500", "l=0.01"), col=c("red","green","blue","yellow","coral"),pch=15)

hist(p1$DISTRIBUTIE, col="red", main="Poisson Distribution") 
hist(p2$DISTRIBUTIE, col=rgb(0,1,0,0.5), add=T)
hist(p3$DISTRIBUTIE, col=rgb(0.2,0.1,0.3,0.5), add=T)
hist(p4$DISTRIBUTIE, col=rgb(0.1,0.05,0.04,0.5), add=T)
hist(p5$DISTRIBUTIE, col=rgb(0.9,0.5,0.23,0.5), add=T)
legend("topright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)


##graficele binomial
b1 = data.frame(DENSITATE=dbinom(0:20, 20, 0.05), MASA=pbinom(0:20, 20, 0.05), DISTRIBUTIE=rbinom(0:20, 20, 0.05))
b2 = data.frame(DENSITATE=dbinom(0:20, 20, 0.1), MASA=pbinom(0:20, 20, 0.1), DISTRIBUTIE=rbinom(0:20, 20, 0.1))
b3 = data.frame(DENSITATE=dbinom(0:20, 20, 0.25), MASA=pbinom(0:20, 20, 0.25), DISTRIBUTIE=rbinom(0:20, 20, 0.25))
b4 = data.frame(DENSITATE=dbinom(0:20, 20, 0.5), MASA=pbinom(0:20, 20, 0.5), DISTRIBUTIE=rbinom(0:20, 20, 0.5))
b5 = data.frame(DENSITATE=dbinom(0:20, 20, 0.75), MASA=pbinom(0:20, 20, 0.75), DISTRIBUTIE=rbinom(0:20, 20, 0.75))

plot(b1$DENSITATE, type="o",col="red", main="Binomial Densitate")
lines(b2$DENSITATE,Type="o",col="green")
lines(b3$DENSITATE,Type="o",col="blue")
lines(b4$DENSITATE,Type="o",col="yellow")
lines(b5$DENSITATE,Type="o",col="coral")
legend("topright",c("p=0.05","p=0.1","p=0.25","p=0.5", "p=0.75"), col=c("red","green","blue","yellow","coral"),pch=15)

plot(b1$MASA,type="o",col="red", main="Binomial Masa")
lines(b2$MASA,type="o",col="green")
lines(b3$MASA,type="o",col="blue")
lines(b4$MASA,type="o",col="yellow")
lines(b5$MASA,type="o",col="coral")
legend("bottomright",c("p=0.05","p=0.1","p=0.25","p=0.5", "p=0.75"), col=c("red","green","blue","yellow","coral"),pch=15)

hist(b1$DISTRIBUTIE, col="red", main="Binomial Distribution") 
hist(b2$DISTRIBUTIE, col=rgb(0,1,0,0.5), add=T)
hist(b3$DISTRIBUTIE, col=rgb(0.2,0.1,0.3,0.5), add=T)
hist(b4$DISTRIBUTIE, col=rgb(0.1,0.05,0.04,0.5), add=T)
hist(b5$DISTRIBUTIE, col=rgb(0.9,0.5,0.23,0.5), add=T)
legend("topright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)


##graficele exponential
e1 = data.frame(DENSITATE=dexp(0:20, 0.25), MASA=pexp(0:20, 0.25), DISTRIBUTIE=rexp(0:20, 0.25))
e2 = data.frame(DENSITATE=dexp(0:20, 0.75), MASA=pexp(0:20, 0.75), DISTRIBUTIE=rexp(0:20, 0.75))
e3 = data.frame(DENSITATE=dexp(0:20, 1), MASA=pexp(0:20, 1), DISTRIBUTIE=rexp(0:20, 1))
e4 = data.frame(DENSITATE=dexp(0:20, 5), MASA=pexp(0:20, 5), DISTRIBUTIE=rexp(0:20, 5))
e5 = data.frame(DENSITATE=dexp(0:20, 20), MASA=pexp(0:20, 20), DISTRIBUTIE=rexp(0:20, 20))

plot(e1$DENSITATE, type="o",col="red", main="Exponential Densitate")
lines(e2$DENSITATE,Type="o",col="green")
lines(e3$DENSITATE,Type="o",col="blue")
lines(e4$DENSITATE,Type="o",col="yellow")
lines(e5$DENSITATE,Type="o",col="coral")
legend("topright",c("r=0.25","r=0.75","r=1","r=5", "r=20"), col=c("red","green","blue","yellow","coral"),pch=15)

plot(e1$MASA,type="o",col="red", main="Exponential Masa") 
lines(e2$MASA,type="o",col="green")
lines(e3$MASA,type="o",col="blue")
lines(e4$MASA,type="o",col="yellow")
lines(e5$MASA,type="o",col="coral")
legend("bottomright",c("r=0.25","r=0.75","r=1","r=5", "r=20"), col=c("red","green","blue","yellow","coral"),pch=15)

hist(e1$DISTRIBUTIE, col="red", main="Exponential Distribution") 
hist(e2$DISTRIBUTIE, col=rgb(0,1,0,0.5), add=T)
hist(e3$DISTRIBUTIE, col=rgb(0.2,0.1,0.3,0.5), add=T)
hist(e4$DISTRIBUTIE, col=rgb(0.1,0.05,0.04,0.5), add=T)
hist(e5$DISTRIBUTIE, col=rgb(0.9,0.5,0.23,0.5), add=T)
legend("topright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)


##graficele normal
n1 = data.frame(DENSITATE=dnorm(0:20, 0, 1), MASA=pnorm(0:20, 0, 1), DISTRIBUTIE=rnorm(0:20, 0, 1))
n2 = data.frame(DENSITATE=dnorm(0:20, 0, 5), MASA=pnorm(0:20, 0, 5), DISTRIBUTIE=rnorm(0:20, 0, 5))
n3 = data.frame(DENSITATE=dnorm(0:20, 5, 1), MASA=pnorm(0:20, 5, 1), DISTRIBUTIE=rnorm(0:20, 5, 1))
n4 = data.frame(DENSITATE=dnorm(0:20, 10, 1), MASA=pnorm(0:20, 10, 1), DISTRIBUTIE=rnorm(0:20, 10, 1))
n5 = data.frame(DENSITATE=dnorm(0:20, 2, 10), MASA=pnorm(0:20, 2, 10), DISTRIBUTIE=rnorm(0:20, 2, 10))

plot(n1$DENSITATE, type="o",col="red", main="Normal Densitate")
lines(n2$DENSITATE,Type="o",col="green")
lines(n3$DENSITATE,Type="o",col="blue")
lines(n4$DENSITATE,Type="o",col="yellow")
lines(n5$DENSITATE,Type="o",col="coral")
legend("topright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)

plot(n1$MASA,type="o",col="red", main="Normal Masa") 
lines(n2$MASA,type="o",col="green")
lines(n3$MASA,type="o",col="blue")
lines(n4$MASA,type="o",col="yellow")
lines(n5$MASA,type="o",col="coral")
legend("bottomright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)

hist(n1$DISTRIBUTIE, col="red", main="Normal Distribution") 
hist(n2$DISTRIBUTIE, col=rgb(0,1,0,0.5), add=T)
hist(n3$DISTRIBUTIE, col=rgb(0.2,0.1,0.3,0.5), add=T)
hist(n4$DISTRIBUTIE, col=rgb(0.1,0.05,0.04,0.5), add=T)
hist(n5$DISTRIBUTIE, col=rgb(0.9,0.5,0.23,0.5), add=T)
legend("topright",c("m=0, sd=1","m=0, sd=5","m=5, sd=1","m=10,sd=1", "m=2,sd=10"), col=c("red","green","blue","yellow","coral"),pch=15)


## ex 4


## 3*2*10 elemente
i <- 1
myk <- numeric(60)
Mass <- numeric(60)
Poison <- numeric(60)
Norm <- numeric(60)
NormCorect <- numeric(60)
CampP <- numeric(60)

for(n in c(25,50,100)){
  for(p in c(0.05, 0.1)){
    for(k in 1:10){
      a <- 1/(9*(n-k))
      b <- 1/(9*(k+1))
      r <- ((k+1)*(1-p))/(p*(n-k))
      c <- (1-b)*r^(1/3)
      miu <- 1-a
      sigmapatrat <- a + (b*r)^(2/3)
      
      myk[i] <- k
      Mass[i] <- pbinom(k, n, p)  ## asta e rezultatul pt care caut aproximari
      DISTRIBUTIE <- rbinom(k,n,p)  
      mylambda <- n*p
      Poison[i] <- ppois(k, lambda = mylambda)
      Norm[i] <-  pnorm((k-n*p)/sqrt(n*p*(1-p)))
      NormCorect[i] <-  pnorm((k + 0.5 - n*p)/sqrt(n*p*(1-p)))
      CampP[i] <- pnorm((c-miu)/sqrt(sigmapatrat))
      i <- i+1
      
    }
  }
}

tabela <- data.frame(k = myk, Binomiala = Mass, Poison = Poison, Normala= Norm, NormalaCorectie= NormCorect, CampPaulson = CampP)
print(tabela)


##ex 5
for(set in 0:5){
  BP <- 0
  BN <- 0
  BNC <- 0
  BCP <- 0
  for(i in 1:10){
    if(abs(tabela[set*10+i,2] - tabela[set*10+i,3]) >= BP )
      BP = abs(tabela[set*10+i,2] - tabela[set*10+i,3])
    
    if(abs(tabela[set*10+i,2] - tabela[set*10+i,4]) >= BN )
      BN = abs(tabela[set*10+i,2] - tabela[set*10+i,4])
    
    if(abs(tabela[set*10+i,2] - tabela[set*10+i,5]) >= BNC )
      BNC = abs(tabela[set*10+i,2] - tabela[set*10+i,5])
    
    if(abs(tabela[set*10+i,2] - tabela[set*10+i,6]) >= BCP)
      BCP = abs(tabela[set*10+i,2] - tabela[set*10+i,6])
    
  }
  
  print(BP)
  print(BN)
  print(BNC)
  print(BCP)
}

plot(tabela$Binomiala, type="o",col="red", main="Binomial approximations")
lines(tabela$Poison,Type="o",col="green")
lines(tabela$Normala,Type="o",col="blue")
lines(tabela$NormalaCorectie,Type="o",col="yellow")
lines(tabela$CampPaulson,Type="o",col="coral")
legend("bottomleft",c("Binomiala","Poison","Normala","Normala Corectie", "Camp Paulson"), col=c("red","green","blue","yellow","coral"),pch=15)



## ex 6

  skewnormal <- function(miu, sigma, lambda) {
    return(Vectorize(function(x) {
      return(2 / sigma *
               dnorm((x - miu) / sigma, mean = 0, sd = 1) * 
               pnorm((lambda * ((x - miu) / sigma)), mean = 0, sd = 1))
    }))
  }
  
  plot(skewnormal(1, 5, 10),col="blue")
  plot(skewnormal(1, 25, 5), add=TRUE, col = "black")
  plot(skewnormal(1, 50, 15), add=TRUE, col = "dark green")
  plot(skewnormal(1, 3, 20), add=TRUE, col = "yellow")
  plot(skewnormal(1, 100, 20), add=TRUE, col = "red")
  legend("topleft", c("5","25","50","3","100"),col=c("blue","black","dark green","yellow", "red"), pch = 10)

  
  
## ex 7
n <- 25
myfunction <- function(n,p){
  dreapta <- (n*p*(1-p))/((1-2*p)^2)
  lambdasquared <- uniroot(function(x)  ((1-(2/pi)*((x)/(1+x)))^3)/((2/pi)*((4/pi-1)^2)*((x/(1+x))^3))-dreapta, c(0,500), extendInt = "yes")$root
  
  lambda <- sign(1-2*p)*sqrt(lambdasquared)
  sigma <- sqrt((n*p*(1-p))/(1-(2/pi)*((lambda^2)/(1+lambda^2))))
  miu <- n*p - sigma*sqrt((2/pi)*((lambda^2)/(1+lambda^2)))
  
  library(sn)
  barplot(dsn(x= 0:n, dp=c(miu,sigma,lambda)),col=rgb(0.3,0.1,0.23,0.5), add=T)
  
}

# pt p=0.05

barplot(pbinom(q=0:n,size=n, prob=0.05), col=rgb(0.9,0.5,0.23,0.5))
myfunction(n,0.05)

# pt p=0.1

barplot(pbinom(q=0:n, size=n, prob=0.1), col=rgb(0.9,0.5,0.23,0.5))
myfunction(n,0.1)


## ex 8
    
    i <- 1
    myk <- numeric(60)
    Binomiala <- numeric(60)
    NormalaAsimetrica <- numeric(60)
    
    for(n in c(25,50,100)){
      for(p in c(0.05, 0.1)){
        for(k in 1:10){
          myk[i] <- k
          Binomiala[i] <- pbinom(k, n, p)  
          dreapta <- (n*p*(1-p))/((1-2*p)^2)
          lambdasquared <- uniroot(function(x)  ((1-(2/pi)*((x)/(1+x)))^3)/((2/pi)*((4/pi-1)^2)*((x/(1+x))^3))-dreapta, c(0,500), extendInt = "yes")$root
          
          lambda <- sign(1-2*p)*sqrt(lambdasquared)
          sigma <- sqrt((n*p*(1-p))/(1-(2/pi)*((lambda^2)/(1+lambda^2))))
          miu <- n*p - sigma*sqrt((2/pi)*((lambda^2)/(1+lambda^2)))
          NormalaAsimetrica[i]<- dsn(x=k, dp=c(miu,sigma,lambda))
          
          i <- i+1
          
        }
      }
    }
    
    tabela2 <- data.frame(k = myk, Binomiala =Binomiala, NormalaAsimetrica= NormalaAsimetrica)
    print(tabela2)
    
   ## afisarea distante Kolomogorov
    for(set in 0:5){
      maxim <- 0
      for(k in 1:10){
        if(abs(tabela2[set*10+k,2] - tabela2[set*10+k,3]) > maxim )
          Kolomogorov = abs(tabela2[set*10+k,2] - tabela2[set*10+k,3])
      }
      print(Kolomogorov)
    }
    

    
    plot(tabela2$Binomiala, type="o",col="red", main="Binomial - Skew Normal approximations")
    lines(tabela2$NormalaAsimetrica,Type="o",col="blue")
    legend("topright",c("Binomiala","Normala Asimetrica"), col=c("red","blue"),pch=15)
    
    