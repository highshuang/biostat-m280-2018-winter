## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime <- function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return(FALSE)
  }
  return(TRUE)
}

# set the seed outside the function
set.seed(seed)

# estimate mean only using observation with prime indices
estMeanPrimes <- function (n, dist, rep) {
  # set seed to generate the same result each time
  
  ind <- sapply(1:n, isPrime)
  sumPrime <- 0
  sumSamp <- 0
  
  # generate data according to the dist input
  # 
  if (dist == "gaussian"){
    for (i in 1:rep){
      x <- rnorm(n)
      sumPrime <- sumPrime + mean(x[ind])^2
      sumSamp <- sumSamp + mean(x)^2
    }
    return(data.frame(sumPrime/rep, sumSamp/rep))
  } else if (dist == "t1"){
    
    for (i in 1:rep){
      x <- rt(n,1)
      sumPrime <- sumPrime + mean(x[ind])^2
      sumSamp <- sumSamp + mean(x)^2
    }
    return(data.frame(sumPrime/rep, sumSamp/rep))
  } else {
    for (i in 1:rep){
      x <- rt(n,5)
      sumPrime <- sumPrime + mean(x[ind])^2
      sumSamp <- sumSamp + mean(x)^2
    }
    return(data.frame(sumPrime/rep, sumSamp/rep))
  }

}

# estimate mean with given 4 paramters
estMeanPrimes(n,dist,rep)







