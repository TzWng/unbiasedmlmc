library(dplyr)
library(ggpubr)
load("multibeta.RData")
y = c(1:8)
y_mean = apply(multibeta,2,mean)
y_sd = apply(multibeta,2,function(y) sd(y)/sqrt(length(y)))
mse = rep(0,8)
for(i in 1:8){
  mse[i] = mean((multibeta[,i] - (i+1))^2)
}
lower = apply(multibeta,2,function(x) quantile(x,0.025))
upper = apply(multibeta,2,function(x) quantile(x,0.975))
df1 = data.frame(y,y_mean,mse,lower,upper)
#figure 3
A = ggplot(data = df1, aes(y,y_mean)) +
  geom_line(aes(x = y, y = y_mean)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=y, ymin=upper, ymax=lower), size = 0.5, width = 0.2) +
  labs(x = "K", y = "Estimates") +
  scale_x_continuous(breaks = seq(1,9,1), limits = c(0.8,8.2)) +
  scale_y_continuous(breaks = seq(0,12,2), limits = c(1,12.5)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A#3.5*5

B = ggplot(data = df1, aes(y,mse)) +
  geom_line(aes(x = y, y = mse)) +
  geom_point() +
  labs(x = "K", y = "Empirical variance") +
  scale_x_continuous(breaks = seq(1,9,1), limits = c(0.8,8.2)) +
  scale_y_continuous(breaks = seq(0,3.6,0.6), limits = c(0,3.6)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B









