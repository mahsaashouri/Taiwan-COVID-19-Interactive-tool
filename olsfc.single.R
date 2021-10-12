olsfc.single <- function(x, h, freq, maxlag = 0, fit) {
  x <- ts(x, frequency = freq, start = 1)
  # Set up data frame for modelling
  n <- length(x)
  if(maxlag > 0)
  {
    lagnames <- paste("Lag", seq(maxlag), sep = '.')
  } 
  else {
    lagnames <- NULL
  }
  # Set up forecast vector
  fc <- ts(numeric(h),
           frequency = frequency(x),
           start = tsp(x)[2] + 1 / frequency(x))
  
  # Set up new data for forecasting
  trend <- length(x) + freq + seq(h)
  season <- factor(cycle(fc))
  newdata <- data.frame(trend = trend[1], season = season[1])
  for (i in seq_along(lagnames))
    newdata[[lagnames[i]]] <- tail(x,i)[1]
  
  # Compute forecasts
  for (i in seq_along(fc))
  {
    fc[i] <- predict(fit, newdata = newdata)
    # Update newdata
    if(maxlag > 0)
    {
      newdata[lagnames[seq(maxlag)]] <- c(fc[i], newdata[lagnames[1:(maxlag-1)]]) 
    }
    newdata[['trend']] <- trend[i+1]
    newdata[['season']] <- season[i+1]
  }
  return(fc)
}

