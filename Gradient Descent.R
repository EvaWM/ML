#' ---
#' title: Gradient Descent
#' author: Evalyne Muiruri
#' #date: January 2019
#' #output: pdf_document
#' #output: html_document
#' output: 
#'  md_document:
#'   variant: markdown_github
#' ---

#' #Gradient Descent
#' Take a small dataset
x <- c(1,2,3,4)
y <- c(24, 29, 34, 50)
plot(x, y)
pars1 <- coefficients(lm(y~x))
abline(pars1, col="red")

#' By linear algebra, the slope of the line is computed to be h(x) = 13.5+8.3x
#' We could try to do this with gradient descent
#' First we need to set up our cost function - mean squared error in this case

mse <- function(x, y, thetas){
  yhat <- thetas + thetas*x
  mse <- sum((yhat-y)^2)/length(x)
  print(mse)
}
mse(x=x, y=y, thetas=pars1)


#' We want to minimise this cost function using gradient descent
gradDesc <- function(x, y, alpha, max.iter, convthresh){
    # where alpha is the learning rate and we fix the no. of iterations we go through to achieve convergence.
  # In this case we consider convergence as when there ceases to be much improvement in the mean squared error.
  
  n <- length(x)
  # initialise parameters
  intercept <- runif(1,0,1)
  slope <- runif(1,0,1)
  yhat <- intercept + slope*x
  error <- sum((yhat-y)^2)/n
  
  iterations = 0 
  # implement gradient descent (using partial derivatives) and with simultaneous update of the parameters
   while(iterations< max.iter){  
     newintercept <- intercept - alpha * ((1 / n) * (sum(yhat-y)))
     newslope <- slope - alpha * ((1 / n) * (sum((yhat-y) * x)))
     slope <- newslope
     intercept <- newintercept
     yhat <- intercept + slope*x
     new_error <- sum((y-yhat)^2)/n
     delta <- error - new_error
     
     if(delta < convthresh) {
       return(cbind(intercept=intercept, slope=slope))
     }
     iterations=iterations+1
     if(identical(iterations,max.iter)){
        warning("Maximum Iterations reached")
        return(cbind(intercept=intercept, slope=slope))
       }
   }
}

gradDesc(x=x, y=y, alpha=0.01, max.iter=50000, convthresh=0.01)

#' And we can see that we get the same outcome as we would have with linear algebra
coefficients(lm(y~x))


