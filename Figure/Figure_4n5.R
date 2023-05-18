load("~ising.RData")
ising_un_02 = ising_un_02[1:n1,]
ising_un_10 = ising_un_10[1:n1,]
x = seq(0.02,0.18,0.01)
x_02 = apply(ising_un_02, 2, mean)
check_02 = check_long_mc[,1]/check_long_mc
true_02 = apply(check_02, 2 ,mean)
x_10 = apply(ising_un_10, 2, mean)
check_10 = check_long_mc[,9]/check_long_mc
true_10 = apply(check_10, 2 ,mean)



estimates <- c(x_02, x_10)
a = rep(seq(0.02,0.18,0.01), 2)
label = c(rep(1, 17), rep(2, 17))
df = data.frame(a, estimates, label)
# Figure 4a
A <- ggplot(data = df, aes(a, estimates)) +
  geom_line(aes(x = a, y = estimates, linetype = factor(label))) +
  geom_point() +
  xlab(expression( theta[range] )) +
  ylab("Estimates") +
  scale_x_continuous(breaks = seq(0.02,0.18,0.04), limits = c(0.015,0.185)) +
  scale_y_continuous(breaks = seq(0,120,30), limits = c(0,120)) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c(expression( Z(theta[range])/Z( "0.02")), 
                                   expression( Z(theta[range])/Z( "0.10") )), 
                        name = NULL)+
  theme_bw(base_size = 16) +
  theme(legend.position = "top", legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
A





mc_02 = ising_mc[,1]/ising_mc
error_un_02 = abs(x_02 - true_02)/true_02
error_mc_02 = (apply(mc_02, 2, mean) - true_02)/true_02
mc_10 = ising_mc[,9]/ising_mc
error_un_10 = abs(x_10 - true_10)/true_10
error_mc_10 = (apply(mc_10, 2, mean) - true_10)/true_10
a = rep(x, 4)
error = c(error_un_02, error_mc_02, error_un_10, error_mc_10)
label = c(rep("Z(0.02)", 34), rep("Z(0.10)", 34))
method = c(rep("Unbiased", 17), rep("Gibbs Sampler", 17), rep("Unbiased", 17), rep("Gibbs Sampler", 17))
df = data.frame(a,error,label,method)
# Figure 4b
B <- ggplot(data = df, aes(a, error)) +
  geom_line(aes(x = a, y = error, linetype = interaction(label, method), colour = interaction(label, method) )) +
  geom_point(aes(colour = interaction(label, method)), size = 0.5) +
  xlab(expression( theta[range] )) +
  ylab("Relative error") +
  scale_x_continuous(breaks = seq(0.02,0.18,0.04), limits = c(0.015,0.185)) +
  scale_color_manual(values = c("red", "red", "black", "black")) + 
  scale_linetype_manual(values = c("solid", "dashed","solid", "dashed")) +
  theme_bw(base_size = 16) + 
  theme(legend.position = c(0.3,0.7), legend.title = element_blank(), legend.key.width= unit(1, 'cm'))
B








un_error_N = abs(cummean((ising_un_10[,17] - true_10[17])/true_10[17]))
mc_error_N = abs(cummean((mc_10[,17] - true_10[17])/true_10[17]))
nprocessor = seq(2,6000,2)
error = c(un_error_N[nprocessor], mc_error_N[nprocessor])
label = c(rep("1",length(nprocessor)), rep("2",length(nprocessor)))
np = rep(nprocessor,2)
df5 = data.frame(np, label, error)
#Figure 5a
C <- ggplot(df5, aes(x = np, y = error)) +
  geom_line(aes(group = label, color = label)) + 
  scale_color_manual(values= c("black", "red"), labels = c("Unbiased", "Gibbs Sampler"), name = NULL) +
  labs(x = 'number of processors', y = 'Relative error') +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.7,0.8),  legend.key.width= unit(1.5, 'cm'))
C






un_error_N = abs(cummean((ising_un_10[,14] - true_10[14])/true_10[14]))
mc_error_N = abs(cummean((mc_10[,14] - true_10[14])/true_10[14]))
nprocessor = seq(12,6000,2)
error = c(un_error_N[nprocessor], mc_error_N[nprocessor])
label = c(rep("1",length(nprocessor)), rep("2",length(nprocessor)))
np = rep(nprocessor,2)
df5 = data.frame(np, label, error)
#Figure 5b
D <- ggplot(df5, aes(x = np, y = error)) +
  geom_line(aes(group = label, color = label)) + 
  scale_color_manual(values= c("black", "red"), labels = c("Unbiased", "Gibbs Sampler"), name = NULL) +
  labs(x = 'number of processors', y = 'Relative error') +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.7,0.8),  legend.key.width= unit(1.5, 'cm'))
D
























