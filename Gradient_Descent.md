<<<<<<< HEAD
Gradient Descent
================

Take a small dataset

``` r
x <- c(1,2,3,4)
y <- c(24, 29, 34, 50)
plot(x, y)
pars1 <- coefficients(lm(y~x))
abline(pars1, col="red")
```

![](Gradient_Descent_files/figure-markdown_github/unnamed-chunk-1-1.png)

By linear algebra, the slope of the line is computed to be h(x) = 13.5+8.3x We could try to do this with gradient descent First we need to set up our cost function - mean squared error in this case

``` r
mse <- function(x, y, thetas){
  yhat <- thetas + thetas*x
  mse <- sum((yhat-y)^2)/length(x)
  print(mse)
}
mse(x=x, y=y, thetas=pars1)
```

    ## [1] 124.515

We want to minimise this cost function using gradient descent

``` r
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
```

    ## Warning in gradDesc(x = x, y = y, alpha = 0.01, max.iter = 50000,
    ## convthresh = 0.01): Maximum Iterations reached

    ##      intercept slope
    ## [1,]      13.5   8.3

And we can see that we get the same outcome as we would have with linear algebra

``` r
coefficients(lm(y~x))
```

    ## (Intercept)           x 
    ##        13.5         8.3
=======
Gradient Descent
================

Take a small dataset

``` r
x <- c(1,2,3,4)
y <- c(24, 29, 34, 50)
plot(x, y)
pars1 <- coefficients(lm(y~x))
abline(pars1, col="red")
```

![](Gradient_Descent_files/figure-markdown_github/unnamed-chunk-1-1.png)

By linear algebra, the slope of the line is computed to be h(x) = 13.5+8.3x We could try to do this with gradient descent First we need to set up our cost function - mean squared error in this case

``` r
mse <- function(x, y, thetas){
  yhat <- thetas + thetas*x
  mse <- sum((yhat-y)^2)/length(x)
  print(mse)
}
mse(x=x, y=y, thetas=pars1)
```

    ## [1] 124.515

We want to minimise this cost function using gradient descent

``` r
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
```

    ## Warning in gradDesc(x = x, y = y, alpha = 0.01, max.iter = 50000,
    ## convthresh = 0.01): Maximum Iterations reached

    ##      intercept slope
    ## [1,]      13.5   8.3

And we can see that we get the same outcome as we would have with linear algebra

``` r
coefficients(lm(y~x))
```

    ## (Intercept)           x 
    ##        13.5         8.3
>>>>>>> bf63980dcd6ce942aedee366c5ed23b5e2057dff
