library(ggplot2)
library(ggplot2)
library(dplyr)
library(magrittr)
library(latex2exp)
library(cowplot)

# Data
load("~/multibeta.RData")



# Figure 2a 
z = seq(0.600,0.800,0.025)
z_mean = apply(beta_dp, 2, mean)
z_error = abs(z_mean-9)/9
z_var = apply(beta_dp, 2, function(y) mean((y-9)^2))
z_sd = sqrt(z_var/n)
df = data.frame(z,z_mean,z_var,z_error,z_sd)
A <- ggplot(df) +
  geom_bar(aes(x = z, y = z_sd-0.005), stat="identity", fill = 'steelblue', position = "dodge", width = 0.01) +
  geom_point(aes(x = z, y = z_error/5 + 4e-4), shape = 4) +
  geom_line(aes(x = z, y = z_error/5 + 4e-4)) +
  labs(x = "Geometric success parameter p", y = "Relative Error") +
  scale_y_continuous(breaks = seq(4e-4, 6e-4, 2e-4), limits = c(0, 6e-4), labels = c("0%","0.1%"),
                     sec.axis = sec_axis(~., name = expression(paste("Standard Error(", 10^-4,")")),
                                         breaks = seq(0, 6e-4, 2e-4), labels = seq(5,5.6,0.2)) ) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A



# Figure 2b 
y = c(1:8)
y_mean = apply(beta_dk,2,mean)
temp = matrix(data = rep(c(2:9),n), nrow = n, byrow = TRUE)
y_var = apply(beta_dk - temp, 2, function(y) {mean(y^2)})
y_sd = sqrt(y_var/n)
df1 = data.frame(y, y_mean, y_var, y_sd)
B <- ggplot(df1) +
  geom_bar(aes(x = y, y = y_sd), stat="identity", fill = 'steelblue', position = "dodge", width = 0.3) +
  geom_point(aes(x = y, y = y_mean/1200)) + 
  geom_line(aes(x = y, y = y_mean/1200)) + 
  labs(x = "K", y = "Estimates") +
  scale_x_continuous(breaks = seq(1,9,1), limits = c(0.8,8.2)) +
  scale_y_continuous(breaks =  seq(1,9,2)/1200, labels = seq(1,9,2), limits = c(0, 8.1e-3),
                     sec.axis = sec_axis(~., name = expression(paste("Standard Error(", 10^-4,")")),
                                         breaks = seq(0, 8.1e-3, 2.7e-3), labels = seq(0,8.1,2.7)) ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B#3.5*5









