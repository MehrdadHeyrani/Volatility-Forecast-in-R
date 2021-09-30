# Volatility-Forecast-in-R

## Volatility Modeling and Volatility Forecast evaluation in R

### ARCH and GARCH Modeleling

### Volatility Forecasting (rollling approach)

```
nObs <- length(y)             # Total Number of observations
from <- seq(1,200)            # In sample vector 
to <- seq(201,414)            # Out of sample  vector fo
Vol_vec <- rep(0,(nObs-200))  # Empty vector for storage of 617 Sigma estimates.
Mean_vec <- rep(0,(nObs-200)) # Empty vector for storage of 617 Mean estimates.

for (i in 1:214){
  # The rolling window of 214 observations.
  data_insert <- y[from[i]:to[i]]
  # Fitting an AR(1)-GARCH(1,1) model with normal cond.dist.
  fitted_model <- garchFit(~ arma(1,0) + garch(1,1), data_insert,
                           trace = FALSE,
                           cond.dist = "sstd")
  # One day ahead forecast of conditional standard deviation.
  prediction_model <- predict(fitted_model, n.ahead = 1)
  Mean_vec[i] <- prediction_model$meanForecast
  Vol_vec[i]  <- prediction_model$standardDeviation
  
  if (length(to)-i != 0){
    print(c('just',(length(to) - i),'iterations left'))
  } else {
    print(c('End!'))
  }
}

plot(y[201:414], type = 'l')
lines(Vol_vec, col = 'red', lwd=3, lty=2)
lines(Mean_vec, col = 'blue', lwd=3,lty=10)

legend("topright", legend = c("return","SD(f)", "Mean(f)"),
       col = c("black","red", "blue"),lwd=c(1,2,2), bty="n", lty=c(1,2,10)) 


```
<img src="https://user-images.githubusercontent.com/77374087/133308479-959c9440-3e83-481e-bd5e-014b564b9161.png" width="600" height="250">

### Volatility Forecast evaluation 

```
accuracy(Mean_vec, y[201:413])
                 ME     RMSE      MAE      MPE     MAPE
Test set -0.2543846 1.684526 1.270509 97.90186 153.2406
```

### using Roll function and foprecast fnction from rugarch Package

