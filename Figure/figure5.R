library(ggplot2)
library(latex2exp)
load("cut_distribution.RData")

A = ggplot(data = df1, aes(y,y_mean)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=y, ymin=upper, ymax=lower), size = 0.5, width = 0.2) +
  labs(x = "d", y = TeX(r'($\lambda^d$)')) +
  scale_x_continuous(breaks = seq(1,13,2), limits = c(0.5,13.5)) +
  scale_y_continuous(breaks = seq(11,29,2), limits = c(11,29)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A#3.5*5



B = ggplot(data = df2, aes(x=V1)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.2,
                 colour = 1, fill = "white") +
  geom_density(alpha=0.5, color = "red") +
  labs(x = TeX(r'($E_{\theta_1}(\max_{d}E_{\theta_1|\theta_1}(\lambda_d))$)'), y = "Density") +
  scale_x_continuous(breaks = seq(10,35,5), limits = c(10,35)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B




