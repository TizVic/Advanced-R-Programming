# Coursera - Advanced R programming course
# Part 1: Factorial Function

library(purrr)
library(microbenchmark)
library(dplyr)

## Function for checking input parameter
checkParamOK <- function(n) {
        return(is.numeric(n) && n >= 0)
}


## version that computes the factorial of an integer using looping
Factorial_loop <- function(n) {
        
        stopifnot(checkParamOK(n))
        if ( n>0 ) {
                res <- 1 # base case
                for (i in 1:n) {
                        res = res * i
                }
                return(res)
        } else {
                return(1) # base case
        }

}

## version that computes the factorial using the reduce() function in the purrr package
# starting from Factorial_reduce(13) reduce produce NA caused by integer overflow 
# in reduce(1:13, `*`)
Factorial_reduce <- function(n) {

        stopifnot(checkParamOK(n))
        if (n>0) {
                return(1:n %>% reduce(`*`))        
        } else {
                return(1)
        }
}

## version that uses recursion to compute the factorial
Factorial_func <- function(n) {

        stopifnot(checkParamOK(n))
        if ( n == 0) {
                return(1) # base case
        } else {
                return(n * Factorial_func(n - 1))
        }       
}

## version that uses memoization to compute the factorial
Factorial_mem <- function(n) {

        stopifnot(checkParamOK(n))
        
        tblMem <- c(1,1,rep(NA,n)) # table for memoize values
        myFactorial_mem <- function(m) {

                if(!is.na(tblMem[m + 1])) {
                        return(tblMem[m + 1])
                } else {
                        return(tblMem[m + 1] <<- m * myFactorial_mem(m - 1))
                }               
                
        }
                
        myFactorial_mem(n)
}

## benckmarking function
evalFuncs <- function(n, nt=1000) {
        output <- microbenchmark(
                sapply(n, Factorial_loop),
                sapply(n, Factorial_reduce),
                sapply(n, Factorial_func),
                sapply(n,Factorial_mem),
                times = nt
        )
        return(output)
}

filename <- "./data/factorial_output.txt"
if (!dir.exists("./data"))
        dir.create("./data")
if (file.exists(filename))
        file.remove(filename)

## start saving outputs
sink(filename)

## Start testing range
#
# Number of times of sampling for microbenchmark
numSampling = 100
# set of values for test factorial functions
factNum <- 0:12
cat(paste0(" ---------- factorial elaborated [0:12] -----------", "\n" ))
output <- evalFuncs(factNum, numSampling)
print(output)
#
## End testing range

## Start testing single value
#
for (factNum in seq(from = 10, to = 100, by = 15)) {
        cat(paste0("\n", " ---------- factorial elaborated ",factNum," -----------","\n" ))
        output <- evalFuncs(factNum) # use default times = 1000
        print(output)
}
#
## End testing single value
sink()




