library(dplyr)
library(ggpubr)
load("ising32.RData")
x = seq(0.23,0.40,0.01)
x_mean = apply(p0.7,2,mean)
x_sd = apply(p0.7,2,function(y) var(y)/length(y))
x_meet = apply(meeting,2,median)
a = rep(x,2)
b = c(x_mean,log(x_meet)/2480)
label = c(rep("estimates",18),rep("meeting time",18))
df = data.frame(a=a, b=b, linet = label, lower, upper)

## figure 4(a)
a = ggplot(data = df, aes(a,b)) +
  geom_line(aes(x = a, y = b, linetype = factor(label))) +
  geom_point() +
  labs(x = "Inverse temperature", y = ("ratio of natural statistics")) +
  ylab(expression(paste("Inverse natural statistics(", 10^-3,")"))) +
  scale_x_continuous(breaks = seq(0.22,0.40,0.03), limits = c(0.22,0.41)) +
  scale_y_continuous(breaks = seq(0.0008,0.0024,0.0002), limits = c(0.0008,0.0020), labels = seq(0.8,2.4,0.2),
                     sec.axis = sec_axis(~., name = "log median meeting time", seq(2.4,4.8,length.out = 7)/2480,
                                         labels = paste(seq(2.4,4.8,length.out =7)))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.key.width= unit(1.5, 'cm'))
a #size 3.5*5

## figure 4(b)
b = data.frame(x,x_sd) %>%
  ggplot(aes(x = x, y = x_sd)) +
  geom_point(size = 1, col = "black") +
  geom_line(col = "black") +
  labs(x = "Inverse temperature", y = "Empirical variance") +
  ylab(expression(paste("Empirical variance(", 10^-16,")"))) +
  scale_x_continuous(breaks = seq(0.22,0.40,0.03), limits = c(0.22,0.41)) +
  scale_y_continuous(breaks = seq(1e-16,2.2e-16,3e-17), limits = c(9e-17,2.3e-16),
                     labels = seq(1,2.2,0.3)) +
  theme_bw(base_size = 12)
b #size 3.5*5
