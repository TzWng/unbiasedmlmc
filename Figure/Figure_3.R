library(ggplot2)
library(ggplot2)
library(dplyr)
library(magrittr)
library(latex2exp)
library(cowplot)

# Data
load("~/multibeta.RData")
source("~/Box_legend.R")

# Figure 3a 
Estimates = c(un_mcmc$unbiased, un_mcmc$mcmc)
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
  geom_hline(yintercept= 9, linetype="dashed", color = "red") +
  labs(x = NULL) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))


C <- plot_grid(C, legend_plot, nrow = 1, rel_widths = c(.75,.25))
C


# Figure 3b
mc_mean = mean(un_mcmc$mcmc)
un_error_N = abs(cummean((un_mcmc$unbiased[1:20000] - 9)/9))
mc_error_N = abs(cummean((un_mcmc$mcmc[1:20000] - 9)/9))
nprocessor = seq(40,8000,10)
error = c(un_error_N[nprocessor], mc_error_N[nprocessor])
label = c(rep("1",length(nprocessor)), rep("2",length(nprocessor)))
np = rep(nprocessor,2)
df3 = data.frame(np, label, error)
D <- ggplot(df3, aes(x = np, y = error)) +
  geom_line(aes(group = label, color = label)) + 
  scale_color_manual(values= c("black", "red"), labels = c("Unbiased", "Metropolis Hastings"), name = NULL)+
  labs(x = 'number of processors', y = 'Relative error(%)') +
  scale_y_continuous(breaks = seq(0, 0.02, 0.005), labels = seq(0.00, 2, 0.5)) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.7,0.8),  legend.key.width= unit(1.5, 'cm'))
D




