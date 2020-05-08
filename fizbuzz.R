
# Task
# Write a program that prints the integers from   1   to   100   (inclusive).
# 
# 
# But:
#   
# for multiples of three,   print   Fizz     (instead of the number)
# for multiples of five,   print   Buzz     (instead of the number)
# for multiples of both three and five,   print   FizzBuzz     (instead of the number)

## https://rosettacode.org/wiki/FizzBuzz

title <- 'FizzBuzz'
#Consigna: Find the largest palindrome made from the product of two 3-digit numbers.

ElapsedTime <- system.time({
    ##########################  

  top     <- 100
  fact3   <- seq(0,top,3)
  fact5   <- seq(0,top,5)
  fact5y3 <-c()
    for (i in 1:top){
      if(i%%3==0 & i%%5==0){fact5y3 <-c(fact5y3,i)}
  }
  
  x         <- 1:top
  x[fact3]  <- 'fizz'
  x[fact5]  <- 'buzz'
  x[fact5y3]<- 'fizzbuzz'

  resultado 

  })[3]
ElapsedMins <- floor(ElapsedTime/60)
ElapsedSecs <- (ElapsedTime-ElapsedMins*60)
cat(sprintf("\nLa respuesta al problema- %s - es:  %s\nTiempo de procesamiento:  %d minutos y %f segundos\n",
            title, resultado, ElapsedMins, ElapsedSecs))
