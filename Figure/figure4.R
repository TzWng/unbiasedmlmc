library(dplyr)
library(ggpubr)
load("ncon_sub1.RData")
load("ncon_sub2.RData")
n = 1e5
x = seq(0.05,0.20,0.01)

#figure 5
x_5 = apply(b5,2,mean)
check5 = apply(check,2,mean)
a = rep(x,2); d5 = c(x_5,check5)
label = c(rep("estimates",16),rep("Gibbs sampler",16))
df5 = data.frame(a, d5, label)
A = ggplot(data = df5, aes(a,d5)) +
  geom_line(aes(x = a, y = d5, linetype = factor(label))) +
  geom_point() +
  xlab(expression( theta[range] )) +
  ylab(expression( Z(theta[range])/Z( theta[1]) )) +
  scale_x_continuous(breaks = seq(0.05,0.20,0.03), limits = c(0.045,0.205)) +
  scale_y_continuous(breaks = seq(0,300,60), limits = c(1,300)) +
  #scale_y_continuous(breaks = seq(0,300,60), limits = c(1,300)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A#size 4*6




x_10 = apply(b10,2,mean)
check10 = check5/check5[6]
d10 = c(x_10,check10)
df10 = data.frame(a, d10, label)
B = ggplot(data = df10, aes(a,d10)) +
  geom_line(aes(x = a, y = d10, linetype = factor(label))) +
  geom_point() +
  xlab(expression( theta[range] )) +
  ylab(expression( Z(theta[range])/Z( theta[2]) )) +
  scale_x_continuous(breaks = seq(0.05,0.20,0.03), limits = c(0.045,0.205)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B



x_15 = apply(b15,2,mean)
check15 = check5/check5[11]
d15 = c(x_15,check15)
df15 = data.frame(a, d15, label)
C = ggplot(data = df15, aes(a,d15)) +
  geom_line(aes(x = a, y = d15, linetype = factor(label))) +
  geom_point() +
  xlab(expression( theta[range] )) +
  ylab(expression( Z(theta[range])/Z( theta[3]) )) +
  scale_x_continuous(breaks = seq(0.05,0.20,0.03), limits = c(0.045,0.205)) +
  scale_y_continuous(breaks = seq(0,15,3), limits = c(0,16)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
C




x_20 = apply(b20,2,mean)
check20 = check5/check5[16]
d20 = c(x_20,check20)
df20 = data.frame(a, d20, label)
D = ggplot(data = df20, aes(a,d20)) +
  geom_line(aes(x = a, y = d20, linetype = factor(label))) +
  geom_point() +
  xlab(expression( theta[range] )) +
  ylab(expression( Z(theta[range])/Z( theta[4]) )) +
  scale_x_continuous(breaks = seq(0.05,0.20,0.03), limits = c(0.045,0.205)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
D





