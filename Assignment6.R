library(ggplot2)
library(qualityTools)


# create a vector of w exponential waiting times with lambda = lam
### create a random vector which is ~Exponential Distribution and length is w
wait <- function(w,lam){
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}

rexp(10,rate=5)
wait(10,5)

# create a vector of exponential waiting times which total t <= Max with lambda = lam
### create a random vector which is ~Exponential Distribution and the sum of them is less than Max
wait.until <- function(Max,lam){
  #set.seed(50)
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])  ##test w seed ## haha use ()
}
###use set.seed to get the same answer.
wait.until(10,5)

# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# (don't forget to comment out the "set.seed")

# poission distribution with size rep
### create a random vector which is a number of cumulation  of the sum of a vector from Exponential distribution given certain Maximum
poi.test <- function(rep, Max, lam){
  #set.seed(50)
  a = NULL
  for(i in 1:rep){
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}
###use set.seed to get the same answer
###comment out he set.seed in wait.until
poi.test(20,2,4)

# now simlate the waiting time for k events to occur with lambda = lam
### create a number which is the sum of a vector from Exponential Distribution given size k
wait.for <- function(k, lam){
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  
  return(time)
} 
wait.for(10,5)

###a vector with size rep & which is from a sum of a vector from Exponential Distribution given size k
gam.test <-function(rep, max.e, lam ){
  a=NULL
  for (i in 1:rep){
    t = wait.for(max.e,lam)
    a = c(a,t)
    
  }
  
  return(a)
}
gam.test(20,2,4) 
  
  
  
  
  
  

