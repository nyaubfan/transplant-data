#===============================================================================
# Simple example from Kruschke's website
#===============================================================================

# Libraries---------------------------------------------------------------------
library(rjags)
library(ggplot2)
library(reshape2)

# View the data-----------------------------------------------------------------
ggplot(plotData,aes(x=InDays,y=Values)) + geom_point()

# BUGS model--------------------------------------------------------------------
JAGS_EXP_DECAY <- "
model {
    # Likelihood
    for( i in 1 : Ndata ) {
        y[i] ~ dnorm( mu[i] , tau )
        mu[i] <- Asym + (R0-Asym)*exp(-exp(lrc)*x[i])
    }
    # Prior distributions
    Asym ~ dnorm(0 , 1.0E-12)
    R0   ~ dnorm(0 , 1.0E-12)
    lrc  ~ dnorm(0, 1.0E-12)
    tau  ~ dgamma(0.001 , 0.001)
}
"

# let the working directory be the current directory of the project-------------
writeLines(BUGS_MLE_MODEL,con="JAGS_EXP_DECAY.txt")

# Generate initial values for the model parameters------------------------------
inits <- function(){
    list(Asym = rnorm(1,0,1E-12),
         R0   = rnorm(1,0,1E-12),
         lrc  = rnorm(1,0,1E-12),
         tau  = rgamma(1,0.001,0.001),
         y    = rnorm(100,0,1))
}

# Set-up the model inputs-------------------------------------------------------
dataList <- list(Ndata=nrow(plotData),x=plotData$InDays,y=plotData$Values)

# Run the model in JAGS---------------------------------------------------------
mle_sims <- jags.model("JAGS_EXP_DECAY.txt",
                       data = dataList,
                       n.chains = 3,
                       n.adapt = 1000)

# Sample from the posterior distribution----------------------------------------
post.samples <- jags.samples(mle_sims,
                             c("Asym","R0","lrc"),
                             1000)

# Inpsect the posterior estimates-----------------------------------------------
postAsym  <- post.samples$Asym[1,,1]
postR0    <- post.samples$R0[1,,1]
postLRC   <- post.samples$lrc[1,,1]

x_fit <- seq(1,35,length.out=1000)
y_hat <- postAsym + (postR0 - postAsym)*exp(-exp(postLRC)*x_fit)

plotPost <- data.frame(postAsym,
                        postR0,
                        postLRC,
                        x_fit,
                        y_hat,
                        Creatnine = plotData$Values,
                        DaysElapsed = plotData$InDays)

ggplot(plot.post,aes(post.beta0,post.beta1)) + geom_point()

plot(Values~InDays,data=plotData,xlim=c(0,40),ylim=c(2,4))
lines(y_hat~x_fit,col="steelblue",lwd=2)





