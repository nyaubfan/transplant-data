#===============================================================================
#
# Tutorial from John Myles White (wwww.johnmyleswhite.com)
#
#===============================================================================

# load libraries----------------------------------------------------------------

library(rjags)
library(ggplot2)
library(reshape2)
library(plyr)

# custom function---------------------------------------------------------------
gotoDir <- function(d){
  return(paste(getwd(),d,sep=""))
}

# Example 1---------------------------------------------------------------------
N <- 10000
x <- rnorm(N,0.0,5)
write.table(
  x
  , file="jmw-example1.data"
  , row.names = FALSE
  , col.names = FALSE
)

# JAGS model
jmw.example1 <- "
model{
    #Likelihood
    for(i in 1:N){
        x[i] ~ dnorm(mu,tau)
    }
    #Priors
    mu ~ dnorm(0,0.0001) # note that in JAGS, precision is used (1/var) or 1/(sd^2)
    tau <- pow(sigma,-2)
    sigma ~ dunif(0,100)
}
"
writeLines(jmw.example1,con="jmw-example1.bug")

# Run the model
jags <- jags.model(
  "jmw-example1.bug"
  , data = list("x" = x, "N" = N)
  , n.chains = 4 
  , n.adapt = 100
)

ex1.sample <- jags.samples(
  jags
  , c("mu","tau")
  , 1000
)
