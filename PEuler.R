

#################################################### 
#         Acumulado de funciones                   #

es.multiplo<- function(x,y)
{
  if (x%%y ==0) return (TRUE)
  else          return (FALSE)
}



#
#################################################### 

#Problema 1
problema <- '1'
title <-'Multiples of 3 and 5'
#Consigna: Find the sum of all the multiples of 3 or 5 below 1000.

ElapsedTime <- system.time({
##########################  
  
  
  seq3<- seq(from = 0, to = 999, by = 3 ) 
  seq5<- seq(from = 0, to = 999, by = 5 ) 
  ab<-c(seq3,seq5)
  x<-unique(ab)
  respuesta<-sum(x)
  

  
##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))


#Problema 2
problema <-'2'
title <- 'Even Fibonacci numbers'
#Consigna: By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

ElapsedTime <- system.time({
  ##########################  
  Fibo<- numeric(33) 
  Fibo[1] <- Fibo[2] <- 1 
  for (i in 3:33) { 
    Fibo[i] <- Fibo[i - 2] + Fibo[i - 1]
  }
    x<-which((Fibo/2)%%1==0) 
    respuesta<-sum(Fibo[x])
  
  
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))


#Problema 3
problema<-'3'
title <-'Largest prime factor'
#Consigna: What is the largest prime factor of the number 600851475143 ?

ElapsedTime <- system.time({
  ##########################  
  
  tope<-600851475143
  f<-sqrt(tope) 
  x<-as.integer(f)
  l<-1:x
  m<-which(tope%%l==0)
  primos=NULL
  for (i in seq_along(m)){
    a<-2:(m[i]-1)
    if(any(m[i]%%a==0)==F){primos=c(primos,m[i]) } 
  }
  respuesta<-max(primos)
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))



#Problema 4
problema <-'4'
title <- 'Largest palindrome product'
#Consigna: Find the largest palindrome made from the product of two 3-digit numbers.

ElapsedTime <- system.time({
  ##########################  
  
  x<-100:999
  productos<-rev(sort(as.vector(x%o%x)))
 
  for (i in 1:length(productos)){
        numero<-productos[i]
    w = paste(rev(substring(numero,1:nchar(numero),1:nchar(numero))),collapse="")
    if (numero == w){    
      resp <- numero ;
      break
    }
  } 
  respuesta<-resp
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))



#Problema 5
problema <- '5'
title <-'Smallest multiple'
#Consigna: What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

ElapsedTime <- system.time({
  ##########################  
  
  n<-20
  
  repeat{
    n<-n+20
    if(any(n%%1:20 != 0)==F)
      break
  }
  respuesta<-n
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))








#Problema 6
problema <- '6'
title<-'Sum square difference'
#Consigna: Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

ElapsedTime <- system.time({
  ##########################  
  
  cuadrados1<-NULL
  cuadrados2<-(sum(1:100))^2
  for (i in 1:100){
      cuadrados1<-c(cuadrados1,(i^2))
    } 
  cuadrados1<-sum(cuadrados1)
  respuesta<-cuadrados2-cuadrados1
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))



#Problema 7
problema <-'7'
title <-'10001st prime'
#Consigna: What is the 10 001st prime number?

ElapsedTime <- system.time({
  ##########################
#   
        ## Defino el tope hasta donde buscar primos
    #if ((M2 <- max(n)) <= 1) # me aseguro que sea mayor a 1 y defino 
    #return(numeric(0))
  
  n<- 1000000
  x <- 1:n
  x[1] <- 0
  
  p <- 1
  
  techo <- floor(sqrt(n))
  
  while((p <- p + 1) <= techo)
  
      if(x[p] != 0){
      x[seq(p^2, n, p)] <- 0
     if (length(x[x > 0])==10001){
       break
     }
      }
 respuesta<- print(x[x > 0][10001])
 
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))




#Problema 8
problema <-'8'
title<- 'Largest product in a series'
#Consigna: Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

ElapsedTime <- system.time({
  ##########################
  
  bignum<-c(7,3,1,6,7,1,7,6,5,3,1,3,3,0,6,2,4,9,1,9,2,2,5,1,1,9,6,7,4,4,2,6,5,7,4,7,4,2,3,5,5,3,4,9,1,9,4,9,3,4,9,6,9,8,3,5,2,0,3,1,2,7,7,4,5,0,6,3,2,6,2,3,9,5,7,8,3,1,8,0,1,6,9,8,4,8,0,1,8,6,9,4,7,8,8,5,1,8,4,3,8,5,8,6,1,5,6,0,7,8,9,1,1,2,9,4,9,4,9,5,4,5,9,5,0,1,7,3,7,9,5,8,3,3,1,9,5,2,8,5,3,2,0,8,8,0,5,5,1,1,1,2,5,4,0,6,9,8,7,4,7,1,5,8,5,2,3,8,6,3,0,5,0,7,1,5,6,9,3,2,9,0,9,6,3,2,9,5,2,2,7,4,4,3,0,4,3,5,5,7,6,6,8,9,6,6,4,8,9,5,0,4,4,5,2,4,4,5,2,3,1,6,1,7,3,1,8,5,6,4,0,3,0,9,8,7,1,1,1,2,1,7,2,2,3,8,3,1,1,3,6,2,2,2,9,8,9,3,4,2,3,3,8,0,3,0,8,1,3,5,3,3,6,2,7,6,6,1,4,2,8,2,8,0,6,4,4,4,4,8,6,6,4,5,2,3,8,7,4,9,3,0,3,5,8,9,0,7,2,9,6,2,9,0,4,9,1,5,6,0,4,4,0,7,7,2,3,9,0,7,1,3,8,1,0,5,1,5,8,5,9,3,0,7,9,6,0,8,6,6,7,0,1,7,2,4,2,7,1,2,1,8,8,3,9,9,8,7,9,7,9,0,8,7,9,2,2,7,4,9,2,1,9,0,1,6,9,9,7,2,0,8,8,8,0,9,3,7,7,6,6,5,7,2,7,3,3,3,0,0,1,0,5,3,3,6,7,8,8,1,2,2,0,2,3,5,4,2,1,8,0,9,7,5,1,2,5,4,5,4,0,5,9,4,7,5,2,2,4,3,5,2,5,8,4,9,0,7,7,1,1,6,7,0,5,5,6,0,1,3,6,0,4,8,3,9,5,8,6,4,4,6,7,0,6,3,2,4,4,1,5,7,2,2,1,5,5,3,9,7,5,3,6,9,7,8,1,7,9,7,7,8,4,6,1,7,4,0,6,4,9,5,5,1,4,9,2,9,0,8,6,2,5,6,9,3,2,1,9,7,8,4,6,8,6,2,2,4,8,2,8,3,9,7,2,2,4,1,3,7,5,6,5,7,0,5,6,0,5,7,4,9,0,2,6,1,4,0,7,9,7,2,9,6,8,6,5,2,4,1,4,5,3,5,1,0,0,4,7,4,8,2,1,6,6,3,7,0,4,8,4,4,0,3,1,9,9,8,9,0,0,0,8,8,9,5,2,4,3,4,5,0,6,5,8,5,4,1,2,2,7,5,8,8,6,6,6,8,8,1,1,6,4,2,7,1,7,1,4,7,9,9,2,4,4,4,2,9,2,8,2,3,0,8,6,3,4,6,5,6,7,4,8,1,3,9,1,9,1,2,3,1,6,2,8,2,4,5,8,6,1,7,8,6,6,4,5,8,3,5,9,1,2,4,5,6,6,5,2,9,4,7,6,5,4,5,6,8,2,8,4,8,9,1,2,8,8,3,1,4,2,6,0,7,6,9,0,0,4,2,2,4,2,1,9,0,2,2,6,7,1,0,5,5,6,2,6,3,2,1,1,1,1,1,0,9,3,7,0,5,4,4,2,1,7,5,0,6,9,4,1,6,5,8,9,6,0,4,0,8,0,7,1,9,8,4,0,3,8,5,0,9,6,2,4,5,5,4,4,4,3,6,2,9,8,1,2,3,0,9,8,7,8,7,9,9,2,7,2,4,4,2,8,4,9,0,9,1,8,8,8,4,5,8,0,1,5,6,1,6,6,0,9,7,9,1,9,1,3,3,8,7,5,4,9,9,2,0,0,5,2,4,0,6,3,6,8,9,9,1,2,5,6,0,7,1,7,6,0,6,0,5,8,8,6,1,1,6,4,6,7,1,0,9,4,0,5,0,7,7,5,4,1,0,0,2,2,5,6,9,8,3,1,5,5,2,0,0,0,5,5,9,3,5,7,2,9,7,2,5,7,1,6,3,6,2,6,9,5,6,1,8,8,2,6,7,0,4,2,8,2,5,2,4,8,3,6,0,0,8,2,3,2,5,7,5,3,0,4,2,0,7,5,2,9,6,3,4,5,0)

  a<-1:13
  
  j<-NULL
  
  while(length(j)<987){
  
  j<-c(j, prod(bignum[a]))
 
  a<-a+1

  }     

  
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))





#Problema 9
problema <-'9'
title<- '	Special Pythagorean triplet'
#Consigna: There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
ElapsedTime <- system.time({
  ##########################
  
  top<-floor(sqrt(998))
  
  repeat{
  m<-sample(1:top, size=1, replace = FALSE)
  n<-sample(1:top, size=1, replace = FALSE)
  
  if (m>n){
  a=m^2-n^2
  b=2*m*n
  c=m^2+n^2
  
  if (a+b+c == 1000){
        break
      }
    }
  
  }
  
  a+c+b
  a^2+b^2==c^2
  respuesta<-a*b*c
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))




#Problema 10
problema<-'10'
title <- 'Summation of primes'

#Consigna: Find the sum of all the primes below two million.

ElapsedTime <- system.time({
  ##########################
  
  n<- 2000000
  x <- 1:n
  x[1] <- 0
  p <- 1
  techo <- floor(sqrt(n))
  while((p <- p + 1) <= techo)
    
    if(x[p] != 0){
      x[seq(p^2, n, p)] <- 0
    }
  respuesta<- sum(x[x > 0])
  
  
  
#   
#   numbrs<-c(2:2000000)
#   for(i in 1:length(numbrs)){
#     if(numbrs[i]!=0){numbrs[seq(i+numbrs[i],length(numbrs),by=numbrs[i])]=0}
#   }
#   sum(numbrs)
  
  ##########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))





for(C in c(997:335)){
  for(B in floor(sqrt(C^2)):20){
    A = sqrt( (C^2) - (B^2) )
    if(A+B+C==1000 & A%%1==0 & A!=0){print(A*B*C);break}
  }
}




#Problema 11
problema <- '11'
title<- 'Largest product in a grid'

#Consigna: 

# In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

# 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
# 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
# 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
# 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
# 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
# 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
# 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
# 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
# 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
# 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
# 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
# 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
# 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
# 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
# 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
# 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
# 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
# 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
# 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
# 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

#What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20�20 grid?
ElapsedTime <- system.time({
#   ##########################

  matrix1<- c(8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8,
              49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0,
              81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65,
              52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91,
              22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80,
              24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50,
              32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70,
              67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21,
              24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72,
              21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95,
              78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92,
              16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57,
              86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58,
              19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40,
              4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66,
              88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69,
              4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36,
              20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16,
              20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54,
              1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48)

m1 <- t(matrix(matrix1, ncol=20, nrow=20))
m2 <- matrix(matrix1, ncol=20, nrow=20)

m1a<-as.vector(m1) 
m2a<-as.vector(m2) 

# las otras
# create an indicator for all diagonals in the matrix
d <- row(m1) - col(m1)
diagonales<-split(m1, d)
d1<-unlist(diagonales,use.names = FALSE)
d2<-d1[6:length(d1)-6] #corto las puntas que no llegaban a tener 4 val

da <- row(m2) - col(m2)
diagonalesa<-split(m2, da)
d1a<-unlist(diagonalesa,use.names = FALSE)
d2a<-d1a[6:length(d1a)-6] #corto las puntas que no llegaban a tener 4 val



resultados<-c()

for(i in 1:length(d2)){
  
  resultados<-c(resultados,prod(d2[1+i:4+i]))
}  

for(i in 1:length(d2a)){
  
  resultados<-c(resultados,prod(d2a[1+i:4+i]))
}
  
for(i in 1:length(m1a)){
  
  resultados<-c(resultados,prod(m1a[1+i:4+i]))
}

for(i in 1:length(m2a)){
  
  resultados<-c(resultados,prod(m2a[1+i:4+i]))
}

max(resultados)

respuesta<-max(resultados,na.rm=TRUE)
respuesta  
###########################  
})[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)

cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))

#Problema 12
problema <- '12'
title <- 'Highly divisible triangular number'
#Consigna: 

# The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
# 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
# Let us list the factors of the first seven triangle numbers:
#  1: 1
#  3: 1,3
#  6: 1,2,3,6
# 10: 1,2,5,10
# 15: 1,3,5,15
# 21: 1,3,7,21
# 28: 1,2,4,7,14,28
# We can see that 28 is the first triangle number to have over five divisors.

# What is the value of the first triangle number to have over five hundred divisors?


ElapsedTime <- system.time({
  ##########################
  
 # Definir fórmula de Nt
 steps <- 600
 Nt <- matrix(NA,nrow=steps, ncol=2)
 
 for (i in 0:steps){
 
 Nt[i,]<- c(i,i*(i+1)/2)

 }
 print(Nt)
#Tengo los num triang

#respuesta del largest prime factors


 
 # Es múltiplo
 # Test pocos números

            ##########################  
})[3]
  ElapsedMins <- floor(ElapsedTime/60)
  ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema %s - %s - es:  %f\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            problema, title, respuesta, ElapsedMins, ElapsedSecs))
  

