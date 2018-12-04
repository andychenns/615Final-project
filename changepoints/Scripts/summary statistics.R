sum.stat <- function(data){
 mean <- mean(data) 
 standard.dev <- sd(data)
 skewness <- skewness(data)
 kurtosis <- kurtosis(data)
 rbind(mean,standard.dev,skewness,kurtosis)
} 