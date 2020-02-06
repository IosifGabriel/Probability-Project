# Punctul a

fgamaux <-function(x,a) {
  return (x^(a-1)*exp(-x));
}

fgam <- function(x) {
  if(x==1)
    return (1)
  if(x==1/2)
    return (sqrt(pi))
  if(x%%1==0 && x>0)
    return (factorial(x-1))
  if(x>1)
    return ((x-1)*fgam(x-1))
  return (integrate(fgamaux, 0, Inf,a=x)$value)
}

fgam(0.2)
gamma(0.2)

# Punctul b

fbet <-function(a,b) {
  if(a+b==1 && a>0 && b>0)
    return (pi/sin(a*pi))
  return (fgam(a)*fgam(b)/fgam(a+b))
}

beta(2,4)
fbet(2,4)

# Punctul c

fdistgamma <- function(x,a,b) {
  if(x>0 && a>0 && b>0)
    return ((x^(a-1)*exp(-x/b))/(b^a*fgam(a)))
  return (0)
}

fdistbeta <-function(x,a,b) {
  if(0<x && x<1 && a>0 && b>0)
    return ((x^(a-1)*(1-x)^(b-1))/(fbet(a,b)))
  return (0)
} 

# 1) P(X < 3)
fprobgamma1 <- function(A,B) {
  return (integrate(fdistgamma,0,3,a=A,b=B)$value)
}

# 2) P(2 < X < 5)
fprobgamma2 <- function(A,B) {
  F1 <- integrate(fdistgamma,0,5,a=A,b=B)
  F2 <- integrate(fdistgamma,0,2,a=A,b=B)
  return (F1$value-F2$value)
}

# 3) P(3 < X < 4 | X > 2)
fprobgamma3 <- function(A,B) {
  F1 <- integrate(fdistgamma, 0, 4,a=A,b=B)$value
  F2 <- integrate(fdistgamma, 0, 3,a=A,b=B)$value
  F3 <- integrate(fdistgamma, 0, 2,a=A,b=B)$value
  return (F1-F2)/(1-F3)
}

# 4) P(Y > 2)
fprobbeta4 <-function(A,B) {
  F1 <- integrate(Vectorize(fdistbeta),0,2,a=A,b=B)$value
  return (1 - F1)
}

# 5) P(4 < X < 6)
fprobgamma5 <- function(A,B) {
  F1 <- integrate(fdistgamma, 0, 6,a=A,b=B)$value
  F2 <- integrate(fdistgamma, 0, 4,a=A,b=B)$value
  return (F1-F2)
}

# 6) P(0 < X < 1 | X < 7)
fprobgamma6 <-function(A,B) {
  F1 <- integrate(Vectorize(fdistgamma), 0, 1,a=A,b=B)$value
  F2 <- integrate(Vectorize(fdistgamma), 0, 0,a=A,b=B)$value
  F3 <- integrate(Vectorize(fdistgamma), 0, 7,a=A,b=B)$value
  return ((F1-F2)/F3)
}

p1 <- fprobgamma1(1,2)
print(p1)
p2 <- fprobgamma2(2,3)
print(p2)
p3 <- fprobgamma3(3,4)
print(p3)
p4 <- fprobbeta4(5,10)
print(p4)
p5 <- fprobgamma5(1,3)
print(p5)
p6 <- fprobgamma6(2,5)
print(p6)

# Punctul d

sdistgamma <- function(x,a,b) {
  if(x>0 && a>0 && b>0)
    return ((x^(a-1)*exp(-x/b))/(b^a*gamma(a)))
  return (0)
}

sdistbeta <-function(x,a,b) {
  if(0<x && x<1 && a>0 && b>0)
    return ((x^(a-1)*(1-x)^(b-1))/(beta(a,b)))
  return (0)
}

# 1) P(X < 3)
sprobgamma1 <- function(A,B) {
  return (integrate(sdistgamma,0,3,a=A,b=B)$value)
}

# 2) P(2 < X < 5)
sprobgamma2 <- function(A,B) {
  F1 <- integrate(sdistgamma,0,5,a=A,b=B)
  F2 <- integrate(sdistgamma,0,2,a=A,b=B)
  return (F1$value-F2$value)
}

# 3) P(3 < X < 4 | X > 2)
sprobgamma3 <- function(A,B) {
  F1 <- integrate(sdistgamma, 0, 4,a=A,b=B)$value
  F2 <- integrate(sdistgamma, 0, 3,a=A,b=B)$value
  F3 <- integrate(sdistgamma, 0, 2,a=A,b=B)$value
  return (F1-F2)/(1-F3)
}

# 4) P(Y > 2)
sprobbeta4 <-function(A,B) {
  F1 <- integrate(Vectorize(sdistbeta),0,2,a=A,b=B)$value
  return (1 - F1)
}

# 5) P(4 < X < 6)
sprobgamma5 <- function(A,B) {
  F1 <- integrate(Vectorize(sdistgamma), 0, 6,a=A,b=B)$value
  F2 <- integrate(Vectorize(sdistgamma), 0, 4,a=A,b=B)$value
  return (F1-F2)
}

# 6) P(0 < X < 1 | X < 7)
sprobgamma6 <-function(A,B) {
  F1 <- integrate(Vectorize(sdistgamma), 0, 1,a=A,b=B)$value
  F2 <- integrate(Vectorize(sdistgamma), 0, 0,a=A,b=B)$value
  F3 <- integrate(Vectorize(sdistgamma), 0, 7,a=A,b=B)$value
  return ((F1 - F2) / F3)
}

s1 <- sprobgamma1(1,2)
print(s1)
s2 <- sprobgamma2(2,3)
print(s2)
s3 <- sprobgamma3(3,3)
print(s3)
s4 <- sprobbeta4(5,10)
print(s4)
s5 <- sprobgamma5(1,3)
print(s5)
s6 <- sprobgamma6(2,5)
print(s6)

vf <- c(p1,p2,p3,p4,p5,p6)
vs <- c(s1,s2,s3,s4,s5,s6)

mat <- cbind(Punctul_C=vf,Punctul_D=vs)
print(mat)