library(ggplot2)
library(qualityTools)


mixfunc<-function(type,len,lam,Max=0)
  ## type is the distribution type, type="exp","poi","gam";
  ## len is the len of vector that we want to creat
  ## lam is the same parameter share by exp-poi-gam distribution
  ## Max is the entry for poi&gam,as poission distribution is the count of times that given sum of exp distribution less than number=Max;
  ## and gamma distribution is the sum of given the count of times of exp distribution until counts to number=Max;
{
  if(len<=0||lam<=0)stop("len & lambda should > 0")
  else if(type=="exp")## choose to give an exp distribution
  {
    set.seed(50)
    a = NULL
    for(i in 1:len)
    {
      a = c(a,rexp(1,rate = lam))## create a number which is ~Exponential Distribution
    }
    return(a)
  }
  else if(type=="poi")## choose to give an poi distribution
  {
  set.seed(50)
  if(Max<=0)stop("invalid input for Max")## if input does not coutian Max, Max =0
    a = NULL
    for(i in 1:len)
    {
      q = wait.until(Max,lam)## function is in the back
      a = c(a,length(q))## each element in poission distribution is length of Exponential Distribution with sum less than Max
    }
    return(a)
  }
  else if(type=="gam")## choose to give a gamma distribution
  {
    set.seed(50)
    if(Max<=0)stop("invalid input for Max") ## if input does not coutian Max, Max =0
    a=NULL
    for (i in 1:len)
    {
      t = sum(rexp(Max,lam))## each element in gamma distribution is sum of Exponential Distribution with length Max
      a = c(a,t)
    }
    return(a)
  }else{stop("type must be exp,poi,gam")} 
}
## this function is included in poi distribution
wait.until <- function(Max,lam)## create a vector which is Exponential Distribution,and sum of them < Max
  {
  time = 0
  b = NULL
  while(time < Max){
    inter = rexp(1,lam)
    b = c(b,inter)
    time = time + inter
  }
  return(b[1:(length(b)-1)])  
}
##test
mixfunc("exp",10,5)
mixfunc("gam",10,5,10)
mixfunc("poi",10,5,10)
##default test
mixfunc("abc",10,5)#wrong type
mixfunc("exp",0,-5)#wrong lambda&length
mixfunc("poi",9,-5)#wrong lambda
mixfunc("gam",10,5)#missing Max
mixfunc("gam","t","u","m")#wrong input
