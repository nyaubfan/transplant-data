#===============================================================================
# Simple example from Kruschke's website
#===============================================================================

# Libraries---------------------------------------------------------------------
library(rjags)
library(ggplot2)
library(reshape2)


# Generate data-----------------------------------------------------------------
Ndata = 5000
b0True = 1.0 #intercept in my regression equation
b1True = 1.0 #coefficient in my regression equation 
sdTrue = 0.5 
set.seed(4740)
xMin=(-3.0) ; xMax=xMin+4
x = rnorm( Ndata , mean=mean(c(xMin,xMax)) , sd=(xMax-xMin)/4 )
y = exp( b1True * ( x - b0True ) ) + rnorm( Ndata , 0 , sdTrue )

# View the data-----------------------------------------------------------------
plot_data <- data.frame(x,y)
ggplot(plot_data,aes(x=x,y=y)) + geom_point()

# BUGS model--------------------------------------------------------------------
BUGS_MLE_MODEL <- "
model {
    # Likelihood
    for( i in 1 : Ndata ) {
        y[i] ~ dnorm( mu[i] , tau )
        mu[i] <- exp( beta1 * ( x[i] - beta0 ) )
    }
    # Prior distributions
    beta0 ~ dnorm( 0 , 1.0E-12 )
    beta1 ~ dnorm( 0 , 1.0E-12 )
    tau ~ dgamma( 0.001 , 0.001 )
}
"

# let the working directory be the current directory of the project-------------
writeLines(BUGS_MLE_MODEL,con="BUGS_MLE_MODEL.txt")

# Generate initial values for the model parameters------------------------------
inits <- function(){
    list(beta0 = rnorm(1,0,1E-12),
         beta1 = rnorm(1,0,1E-12),
         tau = rgamma(1,0.001,0.001),
         y=rnorm(100,0,1))
}

# Set-up the model inputs-------------------------------------------------------
dataList <- list(Ndata=Ndata,x=x,y=y)

# Run the model in JAGS---------------------------------------------------------
mle_sims <- jags.model("BUGS_MLE_MODEL.txt",
                       data = dataList,
                       n.chains = 3,
                       n.adapt = 1000)

# Sample from the posterior distribution----------------------------------------
post.samples <- jags.samples(mle_sims,
                             c("beta0","beta1"),
                             1000)

# Inpsect the posterior estimates-----------------------------------------------
post.beta0 <- post.samples$beta0[1,,1]
post.beta1 <- post.samples$beta1[1,,1]

x_fit <- 1:Ndata
y_hat <- exp( post.beta1[100] * ( x_fit - post.beta0[100] ) )

plot.post <- data.frame(post.beta0,
                        post.beta1,
                        x_fit,
                        y_hat,
                        y,
                        x)

ggplot(plot.post,aes(post.beta0,post.beta1)) + geom_point()

plot(y~x,data=plot.post)
lines(y_hat~x_fit,data=plot.post,col="red",lwd=2)





