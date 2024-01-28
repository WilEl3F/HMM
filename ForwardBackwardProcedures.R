rm(list = ls())

## Forward Procedure
forward_hmm <- function(start_prob, trans_prob, emission_prob){
  a <- matrix(NA,length(start_prob),ncol(emission_prob))
  for(k in 1:length(start_prob)){
    a[k,1] <- start_prob[k]*emission_prob[k,1]
  }
  for(i in 2:ncol(a)){
    for(k in 1:nrow(a)){
      product <- 0
      for(j in 1:nrow(a)){
        product <- product + a[j,i-1]*A[j,k]
      }
      a[k,i] <- product * emission_prob[k,i]
    }
  }
  forward_hmm <- a
}

## Backward Procedure
backward_hmm <- function(start_prob, trans_prob, emission_prob){
  b <- matrix(NA,length(start_prob),ncol(emission_prob))
  b[,ncol(b)] <- 1
  for(i in (ncol(b)-1):1){
    for(j in 1:nrow(b)){
      product <- 0
      for(k in 1:nrow(b)){
        product <- product + A[j,k]*emission_prob[k,i+1]*b[k,i+1]
      }
      b[j,i] <- product
    }
  }
  backward_hmm <- b
}