head(iris)
# Let's make a 90% CI for the mean petal width

library(tidyverse)

boot_vars <- rep(NA, 1000) # where we’ll store the bootstrap variances

for (i in 1:1000) { 
  boot_samp <- iris %>% sample_n(size = length(iris$Petal.Width), replace=TRUE)
  boot_vars[i] <- as.numeric(boot_samp %>% summarize(var(Petal.Length)))  }

quantile(boot_vars, c(0.05, 0.95)) 
## The 90% CI is 0.513 to 0.641


#######################################PLOT
## What is going on?
tibble(variances=boot_vars) %>% 
  ggplot(aes(x=variances)) +
  geom_histogram()+
  geom_vline(xintercept = quantile(boot_vars, c(0.05, 0.95)), color=2)