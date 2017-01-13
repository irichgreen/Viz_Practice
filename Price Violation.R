set.seed(5)

initialPrice = 100  
dailyPlusMinus = 10  
dailyDeviation = dailyPlusMinus/2  
# assumes 2*sigma roughly approximates 95% range on normal dist.

#Note: This isn't the proper way to calculate a standard deviation. We really have no information on how the +/- was calculated,  so it's the best you can do.

n = 31 #number of days in January  
N = 500 #number of simulations

prices = matrix(ncol=N,nrow=n)  
for(i in 1:N){  
prices[,i] = initialPrice + 
cumsum(rnorm(n = n,mean = 0,sd = dailyDeviation))
}

steps = 1:nrow(prices)  
yLimits = c(initialPrice-dailyDeviation*n/1.5,  
initialPrice+dailyDeviation*n/1.5)
plot(steps,prices[,1],type='l',  
ylim=yLimits,xlab='Days',
ylab='Daily Price ($)',
main='Simulation of Daily Prices for the Month')
for(i in 2:ncol(prices)){  
lines(prices[,i])
}


endOfMonthPrices = prices[n,]  
hist(endOfMonthPrices,main='Histogram End of Month Price',  
     xlab='Price ($)',30)


plot(ecdf(endOfMonthPrices),  
     main='CDF of End of Month Price',
     xlab='Price ($)')


