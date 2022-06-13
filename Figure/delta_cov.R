load("Delta_cov.RData")
estimators = cbind(b0509[1:1e5,], b1014[1:1e5,],
                   b1519[1:1e5,], b1620[1:1e5,5])

## Z(0.20)/Z(0.05)
y = estimators[,1]
#1 corresponding to Z(0.05)
#6 corresponding to Z(0.10)
#11 corresponding to Z(0.15)
#16 corresponding to Z(0.20)
x = estimators[,16]
n = length(x)
sigma2 = var(y)/(n*mean(x)^2) - 2*mean(y)*cov(x,y)/(n*mean(x)^3) + var(x)*mean(y)^2/(n*mean(x)^4)
mu = mean(y)/mean(x)
round(mu - 1.96*sqrt(sigma2), 3)
round(mu + 1.96*sqrt(sigma2), 3)
