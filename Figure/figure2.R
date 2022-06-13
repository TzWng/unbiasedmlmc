library(dplyr)
library(ggpubr)
load("K8.RData")
n = 1e5
z = seq(0.600,0.800,0.025)
z_mean = apply(K8,2,mean)
z_sd = apply(K8,2,function(y) mean((y-9)^2))
df = data.frame(z,z_mean,z_sd)
## figure 2
A <- ggplot(df, aes(z,z_mean)) +
  geom_segment(aes(x = z, y = z_mean, xend = z, yend = rep(9,length(z))), linetype = "solid") +
  geom_hline(aes(yintercept=9),linetype = "longdash") +
  labs(x = "Geometric success parameter p", y = "Estimates") +
  scale_x_continuous(breaks = seq(0.55,0.85,0.05), limits = c(0.575,0.825)) +
  scale_y_continuous(breaks = seq(8.996,9.0060,0.002), limits = c(8.9945,9.0060)) +
  theme_bw(base_size = 16)
A

B <- ggplot(df, aes(z,z_sd)) +
  geom_segment(aes(x = z, y = z_sd, xend = z, yend = rep(3.32,length(z))), linetype = "solid") +
  geom_hline(aes(yintercept=3.32),linetype = "longdash") +
  labs(x = "Geometric success parameter p", y = "Empirical variance") +
  scale_x_continuous(breaks = seq(0.55,0.85,0.05), limits = c(0.575,0.825)) +
  scale_y_continuous(breaks = seq(3.40,3.90,0.1), limits = c(3.32,3.84)) +
  theme_bw(base_size = 16)
B
