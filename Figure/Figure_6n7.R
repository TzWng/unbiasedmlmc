library(ggplot2)
library(dplyr)
library(magrittr)
library(latex2exp)
library(cowplot)

load("~/cut_distribution.RData")
source("~/Box_legend.R")
# Figure 6a
A = ggplot(data = df1, aes(y,y_mean)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=y, ymin=upper, ymax=lower), size = 0.5, width = 0.2) +
  labs(x = "d", y = TeX(r'($\lambda^d$)')) +
  scale_x_continuous(breaks = seq(1,13,2), limits = c(0.5,13.5)) +
  scale_y_continuous(breaks = seq(11,29,2), limits = c(11,29)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A


# Figure 6b
B = ggplot(data = unbiased, aes(x=V1)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.2,
                 colour = 1, fill = "white") +
  geom_density(alpha=0.5, color = "red") +
  labs(x = TeX(r'($E_{\theta_1}(\max_{d}E_{\theta_1|\theta_1}(\lambda_d))$)'), y = "Density") +
  scale_x_continuous(breaks = seq(10,35,5), limits = c(10,35)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B



# Figure 7a
n = 1e5
Estimates = c(unbiased$V1[1:n], mcmc$V1[1:n])
label = c(rep("Unbiased",n), rep("Metropolis-Hastings",n))
df2 = data.frame(label,Estimates)

n_fun <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  r[3] <- mean(x)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

legend_plot <- ggplot_box_legend()
C<- ggplot(df2, aes(x=label, y=Estimates)) + 
  # stat_boxplot(geom ='errorbar', width = 0.3) +
  stat_summary(fun.data = n_fun, geom = "boxplot", width = 0.3, fatten = 1) + 
  geom_hline(yintercept= true_mean, linetype="dashed", color = "red") +
  labs(x = NULL) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))


C <- plot_grid(C, legend_plot, nrow = 1, rel_widths = c(.75,.25))
C


# Figure 7b
un_error_N = abs(cummean((unbiased$V1[1:20000] - true_mean)/true_mean))
mc_error_N = abs(cummean((mcmc$V1[1:20000] - true_mean)/true_mean))
nprocessor = seq(180,12000,10)
error = c(un_error_N[nprocessor], mc_error_N[nprocessor])
label = c(rep("1",length(nprocessor)), rep("2",length(nprocessor)))
np = rep(nprocessor,2)
df3 = data.frame(np, label, error)
D <- ggplot(df3, aes(x = np, y = error)) +
  geom_line(aes(group = label, color = label)) + 
  scale_color_manual(values= c("black", "red"), labels = c("Unbiased", "Metropolis Hastings"), name = NULL)+
  labs(x = 'number of processors', y = 'Relative error(%)') +
  scale_y_continuous(breaks = seq(0, 0.04, 0.005), labels = seq(0.00, 4, 0.5)) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.7,0.8),  legend.key.width= unit(1.5, 'cm'))
D




